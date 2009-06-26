/*
 * VManagerModel.java
 *
 * Created on July 14, 2003, 3:50 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;

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
        manager.setMessageFormat(new VMessageFormat(getBundle(getAttributeValue("bundle")).getString(getAttributeValue("format id")))); //$NON-NLS-1$ //$NON-NLS-2$
        manager.setRuleSets(getRuleSets());
    	String s = getAttributeValue("minimum significance"); //$NON-NLS-1$
        try {
        	if(s != null && s.length() > 0) {
        		manager.setMinSignificance(Integer.parseInt(s));
        	}
        } catch (NumberFormatException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        developer = "developer".equals(getAttributeValue("mode")); //$NON-NLS-1$ //$NON-NLS-2$
        manager.addPropertyChangeListener(this);
    }
    
    static Map<String,Object> bundles = new HashMap<String,Object>();
    
    private ResourceBundle getBundle(String baseName) {
        if (baseName == null || baseName.length() == 0) return null;
        if("null".equals(bundles.get(baseName))) return null; //$NON-NLS-1$
        ResourceBundle bundle = (ResourceBundle)bundles.get(baseName);
        if(bundle != null) {
        	return bundle;
        }
        bundle = ResourceBundle.getBundle(baseName);
        Object bo = (bundle == null) ? (Object)"null" : bundle; //$NON-NLS-1$
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
		if(getParent() != null) VHelper.setManager(this);
		//it would be nice to remove this init
		//but then menu will not be initialized
	    if (manager == null && getParent() != null) init();
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
            if ("mode".equals(name)) { //$NON-NLS-1$
                developer = "developer".equals(getAttributeValue("mode")); //$NON-NLS-1$ //$NON-NLS-2$
            } else if ("minimum significance".equals(name)) { //$NON-NLS-1$
                try {
                	if(result != null && result.length() > 0) {
                		manager.setMinSignificance(Integer.parseInt(result));
                	}
                } catch (NumberFormatException e) {
                	ModelPlugin.getPluginLog().logError(e);
                }
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
        return "mode".equals(name) || "minimum significance".equals(name) || super.isAttributeEditable(name); //$NON-NLS-1$ //$NON-NLS-2$
    }
    
    public void propertyChange(PropertyChangeEvent evt) {
        String name = evt.getPropertyName();
        String value = "" + evt.getNewValue(); //$NON-NLS-1$
        if ("minSignificance".equals(name)) { //$NON-NLS-1$
            if (!value.equals(getAttributeValue("minimum significance"))) { //$NON-NLS-1$
                setAttributeValue("minimum significance", value); //$NON-NLS-1$
                setModified(true);
            }
        }
    }
    
}
