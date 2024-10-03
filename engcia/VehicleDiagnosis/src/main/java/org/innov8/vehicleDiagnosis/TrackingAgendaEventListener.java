package org.innov8.vehicleDiagnosis;

import org.drools.core.event.DefaultAgendaEventListener;
import org.kie.api.definition.rule.Rule;
import org.kie.api.event.rule.*;
import org.kie.api.runtime.rule.Match;

import java.util.ArrayList;
import java.util.List;

import org.innov8.model.Fact;
import org.innov8.model.Justification;

@SuppressWarnings("restriction")
public class TrackingAgendaEventListener extends DefaultAgendaEventListener {
    private List<Match> matchList = new ArrayList<Match>();
    private List<Fact> type1 = new ArrayList<Fact>();
    private List<Fact> type2 = new ArrayList<Fact>();

    public void resetType1() {
        type1.clear();
    }

    public void addType1(Fact f) {
        type1.add(f);
    }

    public void resetType2() {
        type2.clear();
    }

    public void addType2(Fact f) {
        type2.add(f);
    }

    @Override
    public void matchCreated(MatchCreatedEvent event) {
    }
    @Override
    public void matchCancelled(MatchCancelledEvent event) {
    }
    @Override
    public void beforeMatchFired(BeforeMatchFiredEvent event) {
    }
    @Override
    public void agendaGroupPushed(AgendaGroupPushedEvent event) {
    }
    @Override
    public void agendaGroupPopped(AgendaGroupPoppedEvent event) {
    }
    @Override
    public void afterMatchFired(AfterMatchFiredEvent event) {
        Rule rule = event.getMatch().getRule();
        String ruleName = rule.getName();

        //System.out.println("Type1:");
        List <Object> list = event.getMatch().getObjects();
        for (Object e : list) {
            if (e instanceof Fact) {
                type1.add((Fact)e);
            }
        }

        /*
        for (Fact f : type1) {
            //System.out.println(f.getId() + ":" + f);
        }
        */

        //System.out.println("Type 2:");
        for (Fact f: type2) {
            //System.out.println(f.getId() + ":" + f);
            Justification j = new Justification(ruleName, type1, f);
            VehicleDiagnosis.justifications.put(f.getId(), j);
        }

        resetType1();
        resetType2();

        /*
        matchList.add(event.getMatch());
        StringBuilder sb = new StringBuilder();
        sb.append("Rule fired: " + ruleName);

        if (ruleMetaDataMap.size() > 0) {
            sb.append("\n  With [" + ruleMetaDataMap.size() + "] meta-data:");
            for (String key : ruleMetaDataMap.keySet()) {
                sb.append("\n    key=" + key + ", value=" + ruleMetaDataMap.get(key));
            }
        }
        */
        //System.out.println(sb.toString());
    }
}
