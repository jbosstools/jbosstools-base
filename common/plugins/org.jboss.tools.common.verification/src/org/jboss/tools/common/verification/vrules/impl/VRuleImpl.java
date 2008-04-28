/*
 * VRuleImpl.java
 *
 * Created on July 14, 2003, 10:51 AM
 */

package org.jboss.tools.common.verification.vrules.impl;

import java.util.*;
import org.jboss.tools.common.verification.vrules.*;
import java.beans.*;

/**
 *
 * @author  valera
 */
public class VRuleImpl implements VRule {
    
    protected String category;
    protected String description;
    protected VEntity[] entities;
    protected String name;
    protected VResult[] results;
    protected boolean enabled;
    protected VAction action;
    protected VRuleSet ruleSet;
    protected int significance;
    protected VResultFactory resultFactory;
    protected PropertyChangeSupport propertyChangeSupport;
    protected Properties properties = null;
    
    /** Creates a new instance of VRuleImpl */
    public VRuleImpl() {
        resultFactory = new VResultFactoryImpl();
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
    
    public String getCategory() {
        return category;
    }
    
    public void setCategory(String category) {
        this.category = category;
    }
    
    public VEntity[] getEntities() {
        return entities;
    }
    
    public void setEntities(VEntity[] entities) {
        this.entities = entities;
    }
    
    public VResult[] getResults() {
        return results;
    }
    
    public void setResults(VResult[] results) {
        this.results = results;
    }
    
    public boolean isEnabled() {
        return enabled;
    }
    
    public void setEnabled(boolean enabled) {
        boolean oldEnabled = this.enabled;
        this.enabled = enabled;
        propertyChangeSupport.firePropertyChange("enabled", oldEnabled, enabled);
    }
    
    public VAction getAction() {
        return action;
    }
    
    public void setAction(VAction action) {
        this.action = action;
        if (action != null) this.action.setRule(this);
    }
    
    public VRuleSet getRuleSet() {
        return ruleSet;
    }
    
    public void setRuleSet(VRuleSet ruleSet) {
        this.ruleSet = ruleSet;
    }
    
    public int getSignificance() {
        return significance;
    }
    
    public void setSignificance(int significance) {
        if (significance > 10) {
            this.significance = 10;
        } else if (significance < 0) {
            this.significance = 0;
        } else {
            this.significance = significance;
        }
    }
    
    public VResultFactory getResultFactory() {
        return resultFactory;
    }
    
    public String getProperty(String name) {
    	return (properties == null) ? null : properties.getProperty(name);
    }
    
    public void setProperties(Properties properties) {
    	this.properties = properties;
    }
    
    /*public VResult[] recheck() {
        VResult[] objs = getResults();
        List res = new ArrayList(objs.length);
        Set checked = new HashSet();
        for (int i = 0; i < objs.length; i++) {
            VObject source = objs[i].getSourceObject();
            if (checked.contains(source)) continue;
            checked.add(source);
            res.addAll(Arrays.asList(action.check(source)));
        }
        return (VResult[])res.toArray(new VResult[res.size()]);
    }*/
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }
}
