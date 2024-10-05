package org.dei;

import org.apache.log4j.BasicConfigurator;
import org.dei.facts.Evidence;
import org.dei.facts.Values;
import org.dei.whynot.DroolsWithWhyNot;
import org.drools.core.ClassObjectFilter;
import org.kie.api.runtime.KieSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class Main {
    static final Logger LOG = LoggerFactory.getLogger(Main.class);

    public static void main(String[] args) {
        BasicConfigurator.configure();  // Logger configurator

        DroolsWithWhyNot drools = DroolsWithWhyNot.init("org.dei.facts");
        KieSession session = drools.getKieSession();

        List<String> ruleSet = new ArrayList<String>();
        session.setGlobal("triggeredRules", ruleSet);
        session.setGlobal("LOG", LOG);

        LOG.info("Now running data");
        Evidence e1 = new Evidence("shape", Values.ROUND);
        session.insert(e1);
        Evidence e2 = new Evidence("diameter", 3);
        session.insert(e2);
        Evidence e3 = new Evidence("color", Values.RED);
        session.insert(e3);
        Evidence e4 = new Evidence("seedcount", 1);
        session.insert(e4);
        session.fireAllRules();

        // Object filter
        LOG.info(">>>>>>>> Evidences:");
        Collection<Evidence> evidenceFacts = (Collection<Evidence>) session.getObjects( new ClassObjectFilter(Evidence.class) );
        for(Evidence e: evidenceFacts) {
            LOG.info(e.toString());
        }

        // Rules triggered:
        LOG.info(">>>>>>>> Rules triggered:");
        for (String r: ruleSet) {
            LOG.info(r);
        }

        // Getting a WhyNot explanation:
        String explanationText = drools.getWhyNotExplanation("Fruit(Values.WATERMELON)");
        System.out.println("Explanation:");
        System.out.println(explanationText);
    }
}
