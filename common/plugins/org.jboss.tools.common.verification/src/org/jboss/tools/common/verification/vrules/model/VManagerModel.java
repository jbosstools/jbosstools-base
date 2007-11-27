/*
 * VManagerModel.java
 *
 * Created on July 14, 2003, 3:50 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.verification.vrules.VHelper;
import org.jboss.tools.common.verification.vrules.VManager;
import org.jboss.tools.common.verification.vrules.VMessageFormat;
import org.jboss.tools.common.verification.vrules.VModel;
import org.jboss.tools.common.verification.vrules.VRuleSet;
import org.jboss.tools.common.verification.vrules.impl.VManagerImpl;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.verification.vrules.layer.VModelImpl;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

/**
 *
 * @author  valera
 */
public class VManagerModel extends RegularObjectImpl implements PropertyChangeListener {
	private static final long serialVersionUID = 2133677518200661058L;
    
    protected VManagerImpl manager;
    protected boolean developer = false;
    
    /** Creates a new instance of VManagerModel */
    public VManagerModel() {
    }
    
    public VManager getManager() {
        if (manager == null) {
            init();
        }
        return manager;
    }
    
    public void init() {
        manager = new VManagerImpl();
        manager.setModel(new VModelImpl(getModel()));
        VHelper.setManager(this);
        manager.setMessageFormat(new VMessageFormat(getBundle(getAttributeValue("bundle")).getString(getAttributeValue("format id"))));
        manager.setRuleSets(getRuleSets());
        try {
            manager.setMinSignificance(Integer.parseInt(getAttributeValue("minimum significance")));
        } catch (NumberFormatException e) {}
        developer = "developer".equals(getAttributeValue("mode"));
        manager.addPropertyChangeListener(this);
    }
    
    static Map<String,Object> bundles = new HashMap<String,Object>();
    
    private ResourceBundle getBundle(String baseName) {
        if (baseName == null || baseName.length() == 0) return null;
        if("null".equals(bundles.get(baseName))) return null;
        ResourceBundle bundle = (ResourceBundle)bundles.get(baseName);
        if(bundle != null) {
        	return bundle;
        }
        try {
            bundle = ResourceBundle.getBundle(baseName);
        } catch (RuntimeException e) {
        	if(VerificationPlugin.isDebugEnabled()) {
        		VerificationPlugin.getPluginLog().logError(e);
        	}
        }
        Object bo = (bundle == null) ? (Object)"null" : bundle;
        bundles.put(baseName, bo);
        return bundle;
    }

    private VRuleSet[] getRuleSets() {
        XModelObject[] c = getChildren();
        VRuleSet[] ruleSets = new VRuleSet[c.length];
        for (int i = 0; i < c.length; i++) {
            ruleSets[i] = ((VRuleSetModel)c[i]).getRuleSet();
        }
        return ruleSets;
    }
    
    protected void loadChildren() {
        try {
        	if(getParent() != null) VHelper.setManager(this);
        	//it would be nice to remove this init
        	//but then menu will not be initialized
            if (manager == null && getParent() != null) init();
        } catch (Exception e) {
			if(VerificationPlugin.isDebugEnabled()) {
				ModelPlugin.getPluginLog().logError(e);
			}
        }
    }

    public boolean addChild(XModelObject child) {
        boolean res = super.addChild(child);
        if (res && manager != null) {
            VRuleSet ruleSet = ((VRuleSetModel)child).getRuleSet();
            manager.loadRuleSet(ruleSet);
        }
        return res;
    }
    
    public void removeChild(XModelObject child) {
        super.removeChild(child);
        if (manager != null) {
            VRuleSet ruleSet = ((VRuleSetModel)child).getRuleSet();
            manager.unloadRuleSet(ruleSet);
    		VModel vmodel = VModelFactory.getModel(getModel());
            ((VModelImpl)vmodel).removeRuleSetActionList(ruleSet);
        }
    }
    
    public String setAttributeValue(String name, String value) {
        String result = super.setAttributeValue(name, value);
        if (manager != null) {
            if ("mode".equals(name)) {
                developer = "developer".equals(getAttributeValue("mode"));
            } else if ("minimum significance".equals(name)) {
                try {
                    manager.setMinSignificance(Integer.parseInt(result));
                } catch (NumberFormatException e) {}
            }
        }
        return result;
    }
    
    protected Comparator<XModelObject> createComparator() {
        return super.createComparator();
    }

    public boolean isObjectEditable() {
        return developer && super.isObjectEditable();
    }

    public boolean isAttributeEditable(String name) {
        return "mode".equals(name) || "minimum significance".equals(name) || super.isAttributeEditable(name);
    }
    
    public void propertyChange(PropertyChangeEvent evt) {
        String name = evt.getPropertyName();
        String value = "" + evt.getNewValue();
        if ("minSignificance".equals(name)) {
            if (!value.equals(getAttributeValue("minimum significance"))) {
                setAttributeValue("minimum significance", value);
                setModified(true);
            }
        }
    }
    
}
