/*
 * VManagerImpl.java
 *
 * Created on July 14, 2003, 11:37 AM
 */

package org.jboss.tools.common.verification.vrules.impl;

import org.jboss.tools.common.verification.vrules.*;
import java.beans.*;

/**
 *
 * @author  valera
 */
public class VManagerImpl implements VManager {
    
    protected VRuleSet[] ruleSets;
    protected VModel model;
    protected int significance;
    protected VMessageFormat format;
    protected PropertyChangeSupport propertyChangeSupport;
    
    /** Creates a new instance of VManagerImpl */
    public VManagerImpl() {
        propertyChangeSupport = new PropertyChangeSupport(this);
    }
    
    public VModel getModel() {
        return model;
    }
    
    public void setModel(VModel model) {
        this.model = model;
    }
    
    public VRuleSet[] getRuleSets() {
        return ruleSets;
    }
    
    public void setRuleSets(VRuleSet[] ruleSets) {
    	if(this.ruleSets == ruleSets) return;
		deactivateRuleSets(this.ruleSets);
        this.ruleSets = ruleSets;
		activateRuleSets(ruleSets);
    }
    
    public void loadRuleSet(VRuleSet ruleSet) {
        int length = this.ruleSets.length;
        VRuleSet[] newSets = new VRuleSet[length+1];
        System.arraycopy(this.ruleSets, 0, newSets, 0, length);
        newSets[length] = ruleSet;
        this.ruleSets = newSets;
        activateRuleSet(ruleSet);
        if(ruleSet != null) activateRuleSets(ruleSet.getRuleSets());
    }
    
    public void unloadRuleSet(VRuleSet ruleSet) {
        int length = ruleSets.length;
        for (int i = 0; i < length; i++) {
            if (ruleSets[i] == ruleSet) {
                deactivateRuleSet(ruleSet);
                VRuleSet[] newSets = new VRuleSet[length-1];
                System.arraycopy(this.ruleSets, 0, newSets, 0, i);
                System.arraycopy(this.ruleSets, i + 1, newSets, i, length - i - 1);
                this.ruleSets = newSets;
                return;
            }
        }
    }
    
    public void updateRuleSet(VRuleSet ruleSet) {
        unloadRuleSet(ruleSet);
        // do something
        loadRuleSet(ruleSet);
    }

	private void activateRuleSets(VRuleSet[] ruleSets) {
		if(ruleSets == null) return;
		for (int i = 0; i < ruleSets.length; i++) {
			activateRuleSet(ruleSets[i]);
			activateRuleSets(ruleSets[i].getRuleSets());
		}
	}
    
    private void activateRuleSet(VRuleSet ruleSet) {
        VRule[] rules = ruleSet.getRules();
        for (int i = 0; i < rules.length; i++) {
            VEntity[] entities = rules[i].getEntities();
            for (int j = 0; j < entities.length; j++) {
                if(entities[j] != null) entities[j].addRule(rules[i]);
            }
        }
    }
    
	private void deactivateRuleSets(VRuleSet[] ruleSets) {
		if(ruleSets == null) return;
		for (int i = 0; i < ruleSets.length; i++) {
			deactivateRuleSet(ruleSets[i]);
			deactivateRuleSets(ruleSets[i].getRuleSets());
		}
	}

    private void deactivateRuleSet(VRuleSet ruleSet) {
        VRule[] rules = ruleSet.getRules();
        for (int i = 0; i < rules.length; i++) {
            VEntity[] entities = rules[i].getEntities();
            for (int j = 0; j < entities.length; j++) {
				if(entities[j] != null) entities[j].removeRule(rules[i]);
            }
        }
    }
    
    public int getMinSignificance() {
        return significance;
    }
    
    public void setMinSignificance(int significance) {
        int oldSignificance = this.significance;
        this.significance = significance;
        propertyChangeSupport.firePropertyChange("minSignificance", oldSignificance, significance);
    }
    
    public VMessageFormat getMessageFormat() {
        return format;
    }
    
    public void setMessageFormat(VMessageFormat format) {
        this.format = format;
    }
    
    public VTask createTask(VObject object) {
        return new VTaskImpl(this, object);
    }
    
    public VTask createTask(VObject object, VRule[] rules) {
        return new VTaskImpl(object, rules);
    }
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

}
