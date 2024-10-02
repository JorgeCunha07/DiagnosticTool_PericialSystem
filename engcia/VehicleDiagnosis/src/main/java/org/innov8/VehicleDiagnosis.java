package org.innov8;

import org.innov8.model.Evidences;
import org.innov8.model.Conclusion;

import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.rule.LiveQuery;
import org.kie.api.runtime.rule.Row;
import org.kie.api.runtime.rule.ViewChangedEventListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.String;

public class VehicleDiagnosis {
    static final Logger LOG = LoggerFactory.getLogger(VehicleDiagnosis.class);

    public static final void main(String[] args) {
        Evidences evidences = new Evidences();
        evidences.setCoolingSystemLeaks("no");
        evidences.setTurnsOnAndShutsdown("no");
        evidences.setFaultyThermostat("no");
        evidences.setCarIssue("yes");
        evidences.setEngineOverheats("no");
        evidences.setIgnitionSystem("no");
        evidences.setFaultyRadiator("no");
        evidences.setFuelInjectionSystem("no");
        evidences.setStartingMotor("no");
        evidences.setBatteryCheck("no");
        evidences.setCarTurnsOn("yes");
        evidences.setFuelSystemIssue("no");
        evidences.setSecuritySystem("no");

        runEngine(evidences);
    }

    private static void runEngine(Evidences evidences) {
        try {
            // load up the knowledge base
            KieServices ks = KieServices.Factory.get();
            KieContainer kContainer = ks.getKieClasspathContainer();
            final KieSession kSession = kContainer.newKieSession("ksession-rules");
            // session name defined in kmodule.xml"

            // Query listener
            ViewChangedEventListener listener = new ViewChangedEventListener() {
                @Override
                public void rowDeleted(Row row) {
                }

                @Override
                public void rowInserted(Row row) {
                    Conclusion conclusion = (Conclusion) row.get("$conclusion");
                    //System.out.println(">>>" + conclusion.toString());
                    LOG.info(">>>" + conclusion.toString());

                    // stop inference engine after as soon as got a conclusion
                    kSession.halt();

                }

                @Override
                public void rowUpdated(Row row) {
                }

            };
            LiveQuery query = kSession.openLiveQuery("Conclusions", null, listener);

            kSession.insert(evidences);

            kSession.fireAllRules();
            // kSession.fireUntilHalt();

            query.close();

        } catch (Throwable t) {
            t.printStackTrace();
        }
    }
}
