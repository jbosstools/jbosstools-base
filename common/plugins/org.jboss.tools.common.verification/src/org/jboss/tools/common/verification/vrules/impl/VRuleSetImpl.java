/*
 * VRuleSetImpl.java
 *
 * Created on July 14, 2003, 11:28 AM
 */

package org.jboss.tools.common.verification.vrules.impl;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.verification.vrules.*;
import java.beans.*;
import java.util.*;

/**
 *
 * @author  valera
 */
public class VRuleSetImpl implements VRuleSet {
    
    protected String description;
    protected String name;
	VRuleSet parentRuleSet;
    protected VRule[] rules;
	protected VRuleSet[] ruleSets;

    protected String url;
    protected String vendor;
    protected String version;
    protected boolean enabled;
    protected ResourceBundle bundle;
    protected PropertyChangeSupport propertyChangeSupport;
    protected Object managerKey;
    
    /** Creates a new instance of VRuleSetImpl */
    public VRuleSetImpl() {
        propertyChangeSupport = new PropertyChangeSupport(this);
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getDescription() {
        return description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    
    public String getVendor() {
        return vendor;
    }
    
    public void setVendor(String vendor) {
        this.vendor = vendor;
    }
    
    public String getVersion() {
        return version;
    }
    
    public void setVersion(String version) {
        this.version = version;
    }
    
    public String getURL() {
        return url;
    }
    
    public void setURL(String url) {
        this.url = url;
    }
    
    public boolean isEnabled() {
    	if(parentRuleSet != null && !parentRuleSet.isEnabled()) return false;
        return enabled;
    }
    
    public void setEnabled(boolean enabled) {
        boolean oldEnabled = this.enabled;
        this.enabled = enabled;
        propertyChangeSupport.firePropertyChange("enabled", oldEnabled, enabled);
    }
    
	public VRuleSet getParentRuleSet() {
		return parentRuleSet;
	}
	
	public void setParentRuleSet(VRuleSet parentRuleSet) {
		this.parentRuleSet = parentRuleSet;
	}

    public VRule[] getRules() {
        return rules;
    }
    
    public void setRules(VRule[] rules) {
        this.rules = rules;
    }
    
	public VRuleSet[] getRuleSets() {
		return ruleSets;
	}
	
	public void setRuleSets(VRuleSet[] ruleSets) {
		this.ruleSets = ruleSets;		
	}

    public VMessageFormat getMessageFormat(String id) {
        if (bundle == null) return null;
        String pattern = null;
        try {
            pattern = bundle.getString("message."+id);
        } catch (MissingResourceException e) {
        	ModelPlugin.log("Cannot find message-id " + id);
        	pattern = "";
        }
        if (pattern == null) return null;
        VMessageFormat format = new VMessageFormat(pattern);
        format.setParent(VHelper.getManager().getMessageFormat());
        return format;
    }
    
    public ResourceBundle getResourceBundle() {
        return bundle;
    }
    
    public void setResourceBundle(ResourceBundle bundle) {
        this.bundle = bundle;
    }
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }
    
    public void setManagerKey(Object key) {
    	managerKey = key;
    }
    
}
