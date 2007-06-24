/*
 * VRuleSetModel.java
 *
 * Created on July 14, 2003, 3:37 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.*;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.verification.vrules.VEntity;
import org.jboss.tools.common.verification.vrules.VModel;
import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.vrules.VRuleSet;
import org.jboss.tools.common.verification.vrules.impl.VRuleSetImpl;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.verification.vrules.layer.VModelImpl;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

/**
 *
 * @author  valera
 */
public class VRuleSetModel extends RegularObjectImpl implements PropertyChangeListener {
	private static final long serialVersionUID = 2133689216972164607L;
    
    protected VRuleSetImpl ruleSet;
    protected boolean installed = false;
    
    /** Creates a new instance of VRuleSetModel */
    public VRuleSetModel() {
    }

	public VRuleSet getRuleSet() {
		return getRuleSet(null);
	}
    
    public VRuleSet getRuleSet(VRuleSet parent) {
        if (ruleSet == null) {
            ruleSet = new VRuleSetImpl();
            ruleSet.setManagerKey(getModel());
            ruleSet.setName(getAttributeValue("name"));
            ruleSet.setDescription(getAttributeValue("description"));
            ruleSet.setEnabled(Boolean.valueOf(getAttributeValue("enabled")).booleanValue());
            ruleSet.setURL(getAttributeValue("url"));
            ruleSet.setVendor(getAttributeValue("vendor"));
            ruleSet.setVersion(getAttributeValue("version"));
            ruleSet.setResourceBundle(getBundle(getAttributeValue("bundle")));
            ruleSet.setRules(getRules());
			ruleSet.setRuleSets(getRuleSets());
            installed = Boolean.valueOf(getAttributeValue("installed")).booleanValue();
            ruleSet.addPropertyChangeListener(this);
        }
		if(parent != null && ruleSet.getParentRuleSet() == null) ruleSet.setParentRuleSet(parent);
        return ruleSet;
    }
    
    private VRule[] getRules() {
        XModelObject[] c = getChildren("VRule");
        VRule[] rules = new VRule[c.length];
        for (int i = 0; i < c.length; i++) {
            rules[i] = ((VRuleModel)c[i]).getRule(ruleSet);
        }
        return rules;
    }
    
	private VRuleSet[] getRuleSets() {
		XModelObject[] c = getChildren("VRuleSet");
		VRuleSet ruleSet = getRuleSet();
		VRuleSet[] ruleSets = new VRuleSet[c.length];
		for (int i = 0; i < c.length; i++) {
			ruleSets[i] = ((VRuleSetModel)c[i]).getRuleSet(ruleSet);
		}
		return ruleSets;
	}
	
	static Map<String,Object> bundles = new HashMap<String,Object>();
	static Object NULL = new Object();
    
    private static ResourceBundle getBundle(String baseName) {
        if (baseName == null || baseName.length() == 0) return null;
        if(bundles.get(baseName) == NULL) return null;
        ResourceBundle resourceBundle = (ResourceBundle)bundles.get(baseName);
        if(resourceBundle != null) return resourceBundle;
        String bundleLoaderClassName = baseName.substring(0, baseName.lastIndexOf('.') + 1) + "BundleLoader";
        Class c = ModelFeatureFactory.getInstance().getFeatureClass(bundleLoaderClassName);
        if(c != null) {
			try {
				ResourceBundle b = ResourceBundle.getBundle(baseName, Locale.getDefault(), c.getClassLoader());
				if(b != null) {
					bundles.put(baseName, b);
					return b;
				}
			} catch (RuntimeException e) {
				//ignore
			}        	
        }
        bundles.put(baseName, NULL);
        if(VerificationPlugin.isDebugEnabled()) {
			ModelPlugin.log("Resource not found: " + baseName + " by org.jboss.tools.common.verification.vrules.model.VRuleSetModel:getBundle()", new Exception(baseName));
        }
        return null;
    }

    public String setAttributeValue(String name, String value) {
        String result = super.setAttributeValue(name, value);
        if (ruleSet != null) {
            if ("enabled".equals(name)) {
                ruleSet.setEnabled(Boolean.valueOf(result).booleanValue());
            } else if ("name".equals(name)) {
                String oldName = ruleSet.getName();
                ruleSet.setName(result);
        		VModel vmodel = VModelFactory.getModel(getModel());
                ((VModelImpl)vmodel).updateRuleSetActionList(ruleSet, oldName);
            } else if ("installed".equals(name)) {
                installed = Boolean.valueOf(result).booleanValue();
            } else if ("bundle".equals(name)) {
                ruleSet.setResourceBundle(getBundle(result));
            }
        }
        return result;
    }
    
    public boolean addChild(XModelObject child) {
        boolean res = super.addChild(child);
        if (res && ruleSet != null) {
            ruleSet.setRules(getRules());
            ruleSet.setRuleSets(getRuleSets());
            if(child instanceof VRuleSetModel) return true;
            VRule rule = ((VRuleModel)child).getRule(ruleSet);
            VEntity[] entities = rule.getEntities();
            for (int i = 0; i < entities.length; i++) {
            	if (entities[i]!=null) entities[i].addRule(rule);
            }
        }
        return res;
    }
    
    public void removeChild(XModelObject child) {
        super.removeChild(child);
        if (ruleSet != null) {
            ruleSet.setRules(getRules());
            VRule rule = ((VRuleModel)child).getRule(ruleSet);
            VEntity[] entities = rule.getEntities();
            for (int i = 0; i < entities.length; i++) {
            	if (entities[i]!=null) entities[i].removeRule(rule);
            }
    		VModel vmodel = VModelFactory.getModel(child.getModel());
            ((VModelImpl)vmodel).removeRuleAction(rule);
        }
    }
    
    protected Comparator<XModelObject> createComparator() {
        return super.createComparator();
    }

    // TODO: override
    public boolean isObjectEditable() {
        return !installed && super.isObjectEditable();
    }

    public boolean isAttributeEditable(String name) {
        return "enabled".equals(name) || super.isAttributeEditable(name);
    }

    public String getPathPart() {
        return getAttributeValue("name");
    }

    public String getPresentationString() {
        return getAttributeValue("name");
    }

    public void propertyChange(PropertyChangeEvent evt) {
        String name = evt.getPropertyName();
        String value = "" + evt.getNewValue();
        if ("enabled".equals(name)) {
            if (!value.equals(getAttributeValue(name))) {
                setAttributeValue(name, value);
                setModified(true);
            }
        }
    }
    
}
