/*
 * VerifyRuleHandler.java
 *
 * Created on July 17, 2003, 6:26 PM
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
public class VerifyRuleHandler extends VerifyHandler {

    protected VRule rule;
    
    /** Creates a new instance of VerifyRuleHandler */
    public VerifyRuleHandler() {
    }
    
    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if (!isEnabled(object)) return;
		VModel vmodel = VModelFactory.getModel(object.getModel());
        Map<String,List<VRule>> entities = new HashMap<String,List<VRule>>();
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
        VObject vobject = vmodel.getObjectByPath(object.getPath());
        check(entities, vobject, object.getModel());
    }

    public boolean isEnabled(XModelObject object) {
        return rule != null && rule.isEnabled() &&
            rule.getRuleSet().isEnabled() && rule.getAction() != null;
    }

    public VRule getRule() {
        return rule;
    }
    
    public void setRule(VRule rule) {
        this.rule = rule;
    }
    
}
