package org.dei.whynot;

import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.FactHandle;
import org.kie.api.runtime.rule.QueryResults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;

/**
 * Class CheckWorkingMemory is used to verify why a conclusion is false,
 * searching for the causes that that prevented a conclusion to be obtained.
 */
public class CheckWorkingMemory {
    private static CheckWorkingMemory singleton = null;
    private final Logger LOG = LoggerFactory.getLogger(CheckWorkingMemory.class);
    private final KieSession session;
    private final Map<String, String> dynamicQueries;
    private final KnowledgeBase kb;

    private CheckWorkingMemory(KieSession session, KnowledgeBase kb) {
        this.session = session;
        this.kb = kb;
        this.dynamicQueries = kb.getDynamicQueries();
    }

    public static CheckWorkingMemory getInstance() {
        if (singleton == null) {
            throw new AssertionError("You have to call init first");
        }
        return singleton;
    }

    public synchronized static CheckWorkingMemory init(KieSession session, KnowledgeBase kb) {
        if (singleton != null) {
            throw new AssertionError("You already initialized me");
        }
        singleton = new CheckWorkingMemory(session, kb);
        return singleton;
    }

    protected boolean conclusionFromRuleDoesNotExist(String ruleName, String functor, String DRLConclusion) throws Exception {
        RuleWM rule = kb.getRuleByName(ruleName);
        String rhs = rule.getRuleConsequence();
        List<String> ruleConclusions = rule.getConclusionFromRhs(functor);

        if (ruleConclusions.isEmpty()) {
            return false;
        }

        boolean exists = false;
        for (String conclusion : ruleConclusions) {
            Object fact = createObject(functor, conclusion);
            FactHandle fh = this.session.insert(fact);

            QueryResults q;
            try {
                q = session.getQueryResults(dynamicQueries.get(DRLConclusion));
            } catch (RuntimeException e) {
                throw new Exception("Undefined query: " + DRLConclusion);
            }

            this.session.delete(fh);

            if (q.size() > 0) {
                exists = true;
                break;
            }
        }

        return exists;
    }

    private Object createObject(String functor, String conclusion) throws Exception {
        String[] conclusionArgs = conclusion.substring(conclusion.indexOf('(') + 1, conclusion.indexOf(')'))
                .replaceAll("\\s+", "").split(",");

        final String FACTS_PACKAGE = kb.getFactsPackage();
        Class<?> type = Class.forName(FACTS_PACKAGE + "." + functor);

        Constructor<?>[] constructors = type.getDeclaredConstructors();
        if (constructors.length == 0) {
            throw new Exception("Constructor not found: " + FACTS_PACKAGE + "." + functor);
        }

        Object[] arguments = new Object[constructors[0].getParameterCount()];
        Class<?>[] parameterTypes = constructors[0].getParameterTypes();
        for (int i = 0; i < arguments.length; i++) {
            arguments[i] = convertArgument(parameterTypes[i], conclusionArgs[i]);
        }

        return constructors[0].newInstance(arguments);
    }

    private Object convertArgument(Class<?> parameterType, String argument) throws Exception {
        if (parameterType.equals(String.class)) {
            return argument.replaceAll("\"", "");
        } else if (parameterType.equals(int.class)) {
            return Integer.parseInt(argument.trim());
        } else if (parameterType.equals(float.class)) {
            return Float.parseFloat(argument.replace(",", ".").trim());
        } else if (parameterType.equals(double.class)) {
            return Double.parseDouble(argument.replace(",", ".").trim());
        } else if (parameterType.isEnum()) {
            return Enum.valueOf((Class<Enum>) parameterType, argument.substring(argument.lastIndexOf('.') + 1));
        } else {
            throw new Exception("Unsupported parameter type: " + parameterType.getName());
        }
    }

    protected boolean conditionIsFalse(String drlCondition) {
        QueryResults q = session.getQueryResults(dynamicQueries.get(drlCondition));
        return q.size() == 0;
    }
}
