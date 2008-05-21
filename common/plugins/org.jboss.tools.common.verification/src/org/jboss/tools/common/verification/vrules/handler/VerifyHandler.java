/*
 * VerifyHandler.java
 *
 * Created on July 17, 2003, 6:26 PM
 */

package org.jboss.tools.common.verification.vrules.handler;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.*;
import java.util.*;

/**
 *
 * @author  valera
 */
public abstract class VerifyHandler extends AbstractHandler implements org.jboss.tools.common.model.event.XActionAgent {
    
    /** Creates a new instance of VerifyHandler */
    public VerifyHandler() {
    }
    
    protected void check(Map entities, VObject vobject, XModel model) {
        VEntity entity = vobject.getEntity();
        List rules = (List)entities.get(entity.getName());
        if (rules != null) {
            for (int i = 0; i < rules.size(); i++) {
                VRule rule = (VRule)rules.get(i);
                VAction vaction = rule.getAction();
                VResult[] results = vaction.check(vobject);
                mergeResults(model, rule, vobject, results);
            }
        }
        Iterator itr = entities.keySet().iterator();
        boolean found = false;
        while (itr.hasNext()) {
            if (entity.isDescendant((String)itr.next())) {
                found = true;
                break;
            }
        }
        if (found) {
            VObject[] c = vobject.getChildren();
            for (int i = 0; i < c.length; i++) {
                check(entities, c[i], model);
            }
        }
    }

    protected void mergeResults(XModel model, VRule rule, VObject source, VResult[] results) {
        int sign = VHelper.getManager().getMinSignificance();
        //XModelObject manager = model.getRoot().getChildren("VManager")[0];
        //XModelObject ruleSetModel = manager.getChildByPath(rule.getRuleSet().getName());
        //XModelObject ruleModel = ruleSetModel.getChildByPath(rule.getName());
        VResult[] c = rule.getResults();
        List<VResult> list = new ArrayList<VResult>();
        for (int i = 0; i < c.length; i++) {
            if (!c[i].getSourceObject().equals(source)) {
                list.add(c[i]);
            }
        }
        if (results == null || results.length == 0) {
            model.getOut().println(rule.getName()+": OK: "+source);
        } else {
            for (int i = 0; i < results.length; i++) {
                if (results[i].getSignificance() < sign) continue;
                list.add(results[i]);
                //VResultModel resultModel = (VResultModel)model.createModelObject("VResult", new Properties());
                //resultModel.setResult(results[i]);
                //ruleModel.addChild(resultModel);

                model.getOut().println(rule.getName()+": "+results[i].getMessage());
            }
        }
        ((VRuleImpl)rule).setResults((VResult[])list.toArray(new VResult[list.size()]));
    }
    
    public void setListener(org.jboss.tools.common.model.event.XActionAgentListener listener) {
    }
    
    public void stopAction() {
    }
    
}
