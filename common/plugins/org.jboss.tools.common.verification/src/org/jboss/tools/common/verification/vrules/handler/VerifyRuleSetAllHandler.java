/*
 * VerifyRuleSetAllHandler.java
 *
 * Created on July 18, 2003, 11:46 AM
 */

package org.jboss.tools.common.verification.vrules.handler;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.model.*;
import java.util.*;

/**
 *
 * @author  valera
 */
public class VerifyRuleSetAllHandler extends VerifyHandler {
    
    protected VRuleSet ruleSet;
    
    /** Creates a new instance of VerifyRuleSetAllHandler */
    public VerifyRuleSetAllHandler() {
    }
    
    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if (!isEnabled(object)) return;
		VModel vmodel = VModelFactory.getModel(object.getModel());
        Map<String,List<VRule>> entities = new HashMap<String,List<VRule>>();
        VRule[] rules = ruleSet.getRules();
        for (int i = 0; i < rules.length; i++) {
            VRule rule = rules[i];
            if (!rule.isEnabled() || rule.getAction() == null) continue;
            VEntity[] ent = rule.getEntities();
            for (int j = 0; j < ent.length; j++) {
                String name = ent[j].getName();
                List<VRule> r = entities.get(name);
                if (r == null) {
                    r = new ArrayList<VRule>();
                    entities.put(name, r);
                }
                r.add(rule);
            }
        }
        VObject vobject = vmodel.getObjectByPath(object.getPath());
        //object.getModel().getOut().println(ruleSet.getName()+":");
        check(entities, vobject, object.getModel());
    }
        
    public boolean isEnabled(XModelObject object) {
        return ruleSet != null && ruleSet.isEnabled();
    }

    public VRuleSet getRuleSet() {
        return ruleSet;
    }
    
    public void setRuleSet(VRuleSet ruleSet) {
        this.ruleSet = ruleSet;
    }
    
}
