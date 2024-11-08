package org.dei.whynot;

import org.drools.compiler.compiler.DrlParser;
import org.drools.compiler.compiler.DroolsParserException;
import org.drools.compiler.lang.descr.*;
import org.kie.api.KieBase;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.KieRepository;
import org.kie.api.builder.Message;
import org.kie.api.io.Resource;
import org.kie.api.runtime.KieContainer;
import org.kie.internal.builder.conf.LanguageLevelOption;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * KnowledgeBase class is used to represent in memory the contents of the DRL files included in the project,
 * plus a set of dynamically generated queries to evaluate rule conditions.
 */
public class KnowledgeBase {
    private final String factsPackage;
    private final Logger LOG = LoggerFactory.getLogger(KnowledgeBase.class);
    private KieBase kieBase;
    private List<RuleDescr> rulesDescr;
    private Map<String, String> dynamicQueries;
    private final List<String> drlPaths;
    private Map<String, RuleWM> rules;

    /**
     * Constructor for KnowledgeBase.
     *
     * @param factsPackage the package containing the facts
     */
    protected KnowledgeBase(String factsPackage) {
        this.factsPackage = factsPackage;
        this.drlPaths = findDrlFiles();
        this.rulesDescr = getRulesDescriptionFromDRL();
        this.rules = getRulesWM();
        this.dynamicQueries = new HashMap<>();
        this.kieBase = createKieBase();
    }

    /**
     * Gets the facts package.
     *
     * @return the facts package
     */
    protected String getFactsPackage() {
        return factsPackage;
    }

    /**
     * Gets the KieBase.
     *
     * @return the KieBase
     */
    public KieBase getKieBase() {
        return this.kieBase;
    }

    /**
     * Gets the dynamic queries.
     *
     * @return a map of dynamic queries
     */
    protected Map<String, String> getDynamicQueries() {
        return dynamicQueries;
    }

    /**
     * Gets a rule by its name.
     *
     * @param ruleName the name of the rule
     * @return the RuleWM object
     * @throws RuntimeException if the rule is not found
     */
    protected RuleWM getRuleByName(String ruleName) {
        RuleWM rule = rules.get(ruleName);
        if (rule == null) {
            throw new RuntimeException("Unknown rule: " + ruleName);
        }
        return rule;
    }

    /**
     * Creates the KieBase.
     *
     * @return the created KieBase
     * @throws RuntimeException if there is an error creating the KieBase
     */
    private KieBase createKieBase() {
        try {
            KieServices ks = KieServices.Factory.get();
            KieRepository kr = ks.getRepository();
            KieFileSystem kfs = ks.newKieFileSystem();

            String queriesDrl = generateQueries();

            for (String drlPath : drlPaths) {
                Resource resource = ks.getResources().newClassPathResource(drlPath);
                kfs.write(resource);
            }

            kfs.write("org/dei/queries.drl", queriesDrl);

            KieBuilder kb = ks.newKieBuilder(kfs);
            kb.buildAll();

            if (kb.getResults().hasMessages(Message.Level.ERROR)) {
                throw new RuntimeException("Build Errors:\n" + kb.getResults().toString());
            }

            KieContainer kContainer = ks.newKieContainer(kr.getDefaultReleaseId());

            LOG.info("Creating kieBase");
            kieBase = kContainer.getKieBase();

            return kieBase;
        } catch (Exception e) {
            LOG.error("Error creating KieBase: {}", e.getMessage(), e);
            throw new RuntimeException("Error creating KieBase", e);
        }
    }

    /**
     * Finds the DRL files.
     *
     * @return a list of DRL file paths
     */
    private List<String> findDrlFiles() {
        List<String> lst = new ArrayList<>();
        lst.add("org/dei/diagnostic.drl");
        return lst;
    }

    /**
     * Gets the rules description from the DRL files.
     *
     * @return a list of RuleDescr objects
     * @throws RuntimeException if there is an error parsing the DRL files
     */
    private List<RuleDescr> getRulesDescriptionFromDRL() {
        try {
            StringBuilder drlContent = new StringBuilder();
            for (String drlPath : drlPaths) {
                InputStream is = getClass().getClassLoader().getResourceAsStream(drlPath);
                if (is == null) {
                    throw new RuntimeException("DRL file not found: " + drlPath);
                }
                Scanner scanner = new Scanner(is, StandardCharsets.UTF_8.name()).useDelimiter("\\A");
                String content = scanner.hasNext() ? scanner.next() : "";
                drlContent.append(content).append("\n");
            }

            DrlParser parser = new DrlParser(LanguageLevelOption.DRL6);
            PackageDescr pkgDescr = parser.parse(false, drlContent.toString());

            if (pkgDescr == null) {
                throw new RuntimeException("Failed to parse DRL files. Check for syntax errors.");
            }

            return pkgDescr.getRules();
        } catch (DroolsParserException e) {
            throw new RuntimeException("DRL parse error: " + e.getMessage(), e);
        }
    }

    /**
     * Gets the rules working memory.
     *
     * @return a map of RuleWM objects
     */
    private Map<String, RuleWM> getRulesWM() {
        this.rules = new HashMap<>();
        for (RuleDescr rule : rulesDescr) {
            String ruleName = rule.getName();
            this.rules.put(ruleName, new RuleWM(ruleName, rule));
        }
        return rules;
    }

    /**
     * Generates the queries.
     *
     * @return the generated queries as a string
     */
    private String generateQueries() {
        Set<String> condSet = getAllRuleConditionsList();
        Set<String> concSet = getAllRuleActionsList();
        Set<String> consSet = concSet.stream().map(c -> {
            try {
                return convertConstructorToDRL(c);
            } catch (Exception e) {
                LOG.error("Error converting constructor to DRL: {}", e.getMessage());
                return null;
            }
        }).collect(Collectors.toSet());

        condSet.addAll(consSet);

        StringBuilder drl = new StringBuilder(getImportsString() + "\n");
        int n = 0;
        for (String c : condSet) {
            String queryName = "confirmCondition" + ++n;
            drl.append("query ").append(queryName).append("\n").append("\t").append(c).append("\n")
                    .append("end").append("\n");
            dynamicQueries.put(c, queryName);
        }

        return drl.toString();
    }

    /**
     * Gets all rule conditions as a set of strings.
     *
     * @return a set of rule conditions
     */
    private Set<String> getAllRuleConditionsList() {
        return rulesDescr.stream()
                .map(RuleDescr::getLhs)
                .map(AndDescr::getDescrs)
                .flatMap(Collection::stream)
                .filter(descr -> descr instanceof PatternDescr)
                .map(descr -> (PatternDescr) descr)
                .map(p -> {
                    String constraints = p.getConstraint().getDescrs().stream()
                            .map(BaseDescr::getText)
                            .reduce((s1, s2) -> s1 + " , " + s2)
                            .orElse("");
                    return p.getObjectType() + "(" + constraints + ")";
                })
                .collect(Collectors.toSet());
    }

    /**
     * Gets all rule actions as a set of strings.
     *
     * @return a set of rule actions
     */
    private Set<String> getAllRuleActionsList() {
        return rulesDescr.stream()
                .map(RuleDescr::getConsequence)
                .map(Object::toString)
                .map(this::getConstructorCalls).flatMap(Set::stream)
                .collect(Collectors.toSet());
    }

    /**
     * Gets the constructor calls from a string.
     *
     * @param consequent the string containing the constructor calls
     * @return a set of constructor calls
     */
    private Set<String> getConstructorCalls(String consequent) {
        Set<String> set = new HashSet<>();
        Pattern pattern = Pattern.compile("new\\s+([\\w\\.<>]+)\\s*\\(([^;]*)\\)");
        Matcher matcher = pattern.matcher(consequent);
        while (matcher.find()) {
            String className = matcher.group(1).replaceAll("<.*>", "");
            String args = matcher.group(2).replaceAll("\\s+", "");
            set.add(className + "(" + args + ")");
        }
        return set;
    }

    /**
     * Gets the imports string.
     *
     * @return the imports string
     * @throws RuntimeException if there is an error getting the imports
     */
    private String getImportsString() {
        Reflections reflections = new Reflections(this.factsPackage, new SubTypesScanner(false));
        try {
            Set<Class<?>> setObjects = new HashSet<>(reflections.getSubTypesOf(Object.class));
            Set<Class<?>> setEnums = new HashSet<>(reflections.getSubTypesOf(Enum.class));
            String importsObjects = setObjects.stream().map(Class::getName)
                    .collect(Collectors.joining(";\nimport ", "import ", ";\n"));
            String importsEnums = setEnums.stream().map(Class::getName)
                    .collect(Collectors.joining(";\nimport ", "import ", ";\n"));
            return importsObjects + importsEnums;
        } catch (Exception e) {
            throw new RuntimeException("Facts package name incorrectly defined: " + this.factsPackage, e);
        }
    }

    /**
     * Converts a constructor to a DRL string.
     *
     * @param constructor the constructor string
     * @return the DRL string
     * @throws Exception if there is an error converting the constructor
     */
    protected String convertConstructorToDRL(String constructor) throws Exception {
        String objectType = constructor.substring(0, constructor.indexOf('(')).replaceAll("\\s+", "");
        objectType = objectType.replaceAll("<.*>", "");

        // Extract the constructor arguments
        String[] constructorArgs = constructor.substring(constructor.indexOf('(') + 1, constructor.lastIndexOf(')'))
                .replaceAll("\\s+", "").split(",");

        // Get the constructor parameters of the class
        String[] parameters = getConstructorParameters(Class.forName(this.factsPackage + "." + objectType));

        if (parameters != null) {
            if (parameters.length != constructorArgs.length) {
                throw new Exception("Invalid number of parameters in conclusion " + objectType);
            }

            // Concatenate the parameters and values to form the DRL string correctly
            String args = IntStream.range(0, parameters.length)
                    .mapToObj(i -> parameters[i] + " == " + constructorArgs[i])
                    .collect(Collectors.joining(" , "));

            return objectType + "(" + args + ")";
        }

        return objectType + "()";
    }

    /**
     * Gets the constructor parameters of a class.
     *
     * @param c the class
     * @return an array of parameter names
     * @throws Exception if there is an error getting the parameters
     */
    private String[] getConstructorParameters(Class<?> c) throws Exception {
        Constructor<?>[] allConstructors = c.getConstructors();
        if (allConstructors.length == 0) {
            throw new Exception("Undefined constructor for class " + c.getName());
        }
        Parameter[] params = allConstructors[0].getParameters();
        return Arrays.stream(params).map(Parameter::getName).toArray(String[]::new);
    }

    /**
     * Checks if a fact is a basic fact.
     *
     * @param fact the fact to check
     * @return true if the fact is a basic fact, false otherwise
     */
    protected boolean isBasicFact(String fact) {
        final String functor = fact.replaceAll("\\s+", "").split("\\(")[0];
        return rulesDescr.stream()
                .map(RuleDescr::getConsequence)
                .noneMatch(s -> s.toString().contains(functor));
    }

    /**
     * Gets the rules that obtain a conclusion.
     *
     * @param conclusion the conclusion to check
     * @return a list of rule names
     */
    protected List<String> getRulesObtainingConclusion(String conclusion) {
        String regex = "(?s).*" + conclusion + "\\(" + ".*" + "\\)" + ".*";
        return rulesDescr.stream()
                .filter(r -> r.getConsequence().toString()
                        .replaceAll("\\s+", "")
                        .matches(regex))
                .map(RuleDescr::getName)
                .collect(Collectors.toList());
    }

    /**
     * Converts a DRL pattern to a constructor string.
     *
     * @param patt the pattern description
     * @return the constructor string
     * @throws Exception if there is an error converting the pattern
     */
    protected String convertDRLPatternToConstructor(PatternDescr patt) throws Exception {
        final String objectType = patt.getObjectType();
        StringBuilder conclusion = new StringBuilder(objectType + "(");

        Map<String, String> map = patt.getDescrs().stream()
                .map(BaseDescr::getText)
                .map(s -> s.replaceAll("\\s+", ""))
                .collect(Collectors.toMap(k -> k.split("[=:<>]{1,3}")[0], v -> v.split("[=:<>]{1,3}")[1]));

        String[] parameters = getConstructorParameters(Class.forName(this.factsPackage + "." + objectType));
        if (parameters != null) {
            conclusion.append(Arrays.stream(parameters).map(map::get).collect(Collectors.joining(",")));
        }

        conclusion.append(")");

        return conclusion.toString();
    }
}