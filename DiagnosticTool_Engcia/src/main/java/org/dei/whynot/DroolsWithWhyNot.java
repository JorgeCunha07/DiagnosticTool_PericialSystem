package org.dei.whynot;

import lombok.Getter;
import org.kie.api.KieBase;
import org.kie.api.runtime.KieSession;
import org.kie.api.event.rule.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Singleton class for managing Drools sessions with "Why Not" explanations.
 */
public class DroolsWithWhyNot {
    private static DroolsWithWhyNot singleton = null;
    private static WhyNot wn;
    private final Logger LOG = LoggerFactory.getLogger(DroolsWithWhyNot.class);
    private final String factsPackage;
    private KieSession session;
    private KnowledgeBase kb;

    // List of triggered and untriggered rules for whyNot explanation
    @Getter
    private List<String> untriggeredRules = new ArrayList<>();
    @Getter
    private List<String> triggeredRules = new ArrayList<>(); // List of triggered rules

    /**
     * Private constructor for DroolsWithWhyNot.
     *
     * @param factsPackage the package containing the facts
     */
    private DroolsWithWhyNot(String factsPackage) {
        this.factsPackage = factsPackage;
        this.kb = new KnowledgeBase(factsPackage);
        this.session = createKieSession();
    }

    /**
     * Initializes the singleton instance of DroolsWithWhyNot.
     *
     * @param factsPackage the package containing the facts
     * @return the initialized DroolsWithWhyNot instance
     */
    public synchronized static DroolsWithWhyNot init(String factsPackage) {
        if (singleton == null) {
            singleton = new DroolsWithWhyNot(factsPackage);
        }
        return singleton;
    }

    /**
     * Retrieves the singleton instance of DroolsWithWhyNot.
     *
     * @return the DroolsWithWhyNot instance
     * @throws AssertionError if the instance is not initialized
     */
    public static DroolsWithWhyNot getInstance() {
        if (singleton == null) {
            throw new AssertionError("Class " + DroolsWithWhyNot.class.getSimpleName() + " is not initialized.");
        }
        return singleton;
    }

    /**
     * Provides an explanation for why a specific conclusion was not reached.
     *
     * @param expectedConclusion the expected conclusion that was not reached
     * @return a string explanation detailing the untriggered rules
     */
    public String getWhyNotExplanation(String expectedConclusion) {
        return wn.getWhyNotExplanation(expectedConclusion);
    }

    /**
     * Checks if the singleton instance is initialized.
     *
     * @return true if the instance is initialized, false otherwise
     */
    public static boolean isInitialized() {
        return singleton != null;
    }

    /**
     * Retrieves the KieSession.
     *
     * @return the KieSession
     */
    public KieSession getKieSession() {
        return session;
    }

    /**
     * Retrieves the KnowledgeBase.
     *
     * @return the KnowledgeBase
     */
    public KnowledgeBase getKnowledgeBase() {
        return kb;
    }

    /**
     * Creates a new KieSession and adds event listeners to track rule execution.
     *
     * @return the created KieSession
     * @throws RuntimeException if there is an error creating the KieSession
     */
    private KieSession createKieSession() {
        try {
            KieBase kieBase = kb.getKieBase();
            LOG.info("Creating kieSession");
            session = kieBase.newKieSession();

            // Add Agenda Event Listener to track triggered and untriggered rules
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

                @Override
                public void afterMatchFired(AfterMatchFiredEvent event) {
                    LOG.info("Rule {} was fired", event.getMatch().getRule().getName());
                    triggeredRules.add(event.getMatch().getRule().getName()); // Store triggered rules
                }
            });
            return session;
        } catch (Exception e) {
            LOG.error("Failed to create KieSession: {}", e.getMessage(), e);
            throw new RuntimeException("Error creating KieSession", e);
        }
    }

    /**
     * Disposes of the KieSession.
     */
    public void dispose() {
        if (session != null) {
            LOG.info("Disposing kieSession");
            session.dispose();
            session = null;
        }
    }
}