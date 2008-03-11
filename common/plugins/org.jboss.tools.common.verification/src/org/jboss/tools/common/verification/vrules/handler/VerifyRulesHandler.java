/*
 * VerifyRulesHandler.java
 *
 * Created on July 18, 2003, 12:26 AM
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
public class VerifyRulesHandler extends VerifyHandler {
    
    /** Creates a new instance of VerifyRulesHandler */
    public VerifyRulesHandler() {
    }
    
    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if (!isEnabled(object)) return;
		VModel vmodel = VModelFactory.getModel(object.getModel());
        VObject vobject = vmodel.getObjectByPath(object.getPath());
        VRule[] rules = vobject.getEntity().getRules();
        for (int i = 0; i < rules.length; i++) {
            VRule rule = rules[i];
            VAction vaction = rule.getAction();
            if (!rule.isEnabled() || !rule.getRuleSet().isEnabled() || vaction == null) continue;
            VResult[] results = vaction.check(vobject);
            if (results == null || results.length == 0) {
                object.getModel().getOut().println(rule.getName()+": OK");
            } else {
                for (int j = 0; j < results.length; j++) {
                    object.getModel().getOut().println(rule.getName()+": "+results[j].getMessage());
                }
            }
            mergeResults(object.getModel(), rule, vobject, results);
        }
    }

    public boolean isEnabled(XModelObject object) {
        return VHelper.getManager() != null;
    }

}
