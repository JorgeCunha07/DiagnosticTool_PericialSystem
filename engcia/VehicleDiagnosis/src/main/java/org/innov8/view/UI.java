package org.innov8.view;

import org.innov8.vehicleDiagnosis.VehicleDiagnosis;
import org.kie.api.runtime.ClassObjectFilter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;

import org.innov8.model.Evidence;

public class UI {
    private static BufferedReader br;

    public static void uiInit() {
        br = new BufferedReader(new InputStreamReader(System.in));
    }

    public static void uiClose() {
        if (br != null) {
            try {
                br.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static boolean answer(String ev, String v) {
        @SuppressWarnings("unchecked")
        Collection<Evidence> evidences = (Collection<Evidence>) VehicleDiagnosis.KS.getObjects(new ClassObjectFilter(Evidence.class));
        boolean questionFound = false;
        Evidence evidence = null;
        for (Evidence e: evidences) {
            if (e.getEvidence().compareTo(ev) == 0) {
                questionFound = true;
                evidence = e;
                break;
            }
        }
        if (questionFound) {
            if (evidence.getValue().compareTo(v) == 0) {
                VehicleDiagnosis.agendaEventListener.addLhs(evidence);
                return true;
            } else {
                // Clear LHS conditions set if a condition is false (conjunctive rules)
                VehicleDiagnosis.agendaEventListener.resetLhs();
                return false;
            }
        }
        System.out.print(ev + "? ");
        String value = readLine();

        Evidence e = new Evidence(ev, value);
        VehicleDiagnosis.KS.insert(e);

        if (value.compareTo(v) == 0) {
            VehicleDiagnosis.agendaEventListener.addLhs(e);
            return true;
        } else {
            // Clear LHS conditions set if a condition is false (conjunctive rules)
            VehicleDiagnosis.agendaEventListener.resetLhs();
            return false;
        }
    }

    private static String readLine() {
        String input = "";

        try {
            input = br.readLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return input;
    }

}
