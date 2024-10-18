package org.dei.whynot;

import lombok.Getter;
import org.kie.api.KieBase;
import org.kie.api.runtime.KieSession;
import org.kie.api.event.rule.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class DroolsWithWhyNot {
    private static DroolsWithWhyNot singleton = null;
    private static WhyNot wn;
    private final Logger LOG = LoggerFactory.getLogger(DroolsWithWhyNot.class);
    private final String factsPackage;
    private KieSession session;
    private KnowledgeBase kb;
    // Get untriggered rules for whyNot explanation
    @Getter
    private List<String> untriggeredRules = new ArrayList<>();

    private DroolsWithWhyNot(String factsPackage) {
        this.factsPackage = factsPackage;
        this.kb = new KnowledgeBase(factsPackage);
        this.session = createKieSession();
    }

    public synchronized static DroolsWithWhyNot init(String factsPackage) {
        if (singleton != null) {
            throw new AssertionError("Class " + DroolsWithWhyNot.class.getSimpleName() + " was already initialized.");
        }

        singleton = new DroolsWithWhyNot(factsPackage);
        wn = WhyNot.init(singleton);
        return singleton;
    }

    public String getWhyNotExplanation(String expectedConclusion) {
        return wn.getWhyNotExplanation(expectedConclusion);
    }

    public KieSession getKieSession() {
        return session;
    }

    protected KnowledgeBase getKnowledgeBase() {
        return kb;
    }

    private KieSession createKieSession() {
        try {
            KieBase kieBase = kb.getKieBase();
            LOG.info("Creating kieSession");
            session = kieBase.newKieSession();

            // Add Agenda Event Listener to track untriggered rules
            session.addEventListener(new DefaultAgendaEventListener() {
                @Override
                public void matchCreated(MatchCreatedEvent event) {
                    LOG.info("Rule {} was created to be fired", event.getMatch().getRule().getName());
                }

                @Override
                public void matchCancelled(MatchCancelledEvent event) {
                    LOG.info("Rule {} was cancelled and will not fire", event.getMatch().getRule().getName());
                    untriggeredRules.add(event.getMatch().getRule().getName());
                }
            });
            return session;
        } catch (Exception e) {
            LOG.error("Failed to create KieSession: {}", e.getMessage(), e);
            throw new RuntimeException("Error creating KieSession", e);
        }
    }

}
