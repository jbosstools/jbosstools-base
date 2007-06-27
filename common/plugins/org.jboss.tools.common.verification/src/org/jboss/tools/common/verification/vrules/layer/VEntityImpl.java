/*
 * VEntityImpl.java
 *
 * Created on July 11, 2003, 6:56 PM
 */

package org.jboss.tools.common.verification.vrules.layer;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;

import java.util.*;

/**
 *
 * @author  valera
 */
public class VEntityImpl implements VEntity {
	private XModelMetaData meta;
	private String entityName;
    private XModelEntity modelEntity;
    private VModelImpl model;
    private List<VRule> rules;
    
    /** Creates a new instance of VEntityImpl */
    public VEntityImpl(XModelMetaData meta, String entityName, VModelImpl model) {
    	this.meta = meta;
    	this.entityName = entityName;
        this.model = model;
        this.rules = new ArrayList<VRule>();
    }
    
    public String getName() {
        return entityName;
    }
    
    public VEntity[] getChildren() {
        XChild[] c = getModelEntity().getChildren();
        VEntity[] children = new VEntity[c.length];
        for (int i = 0; i < c.length; i++) {
            children[i] = model.getEntity(c[i].getName());
        }
        return children;
    }
    
    public VRule[] getRules() {
        return (VRule[])rules.toArray(new VRule[rules.size()]);
    }
    
    public void addRule(VRule rule) {
    	if(rules.contains(rule)) return;
        rules.add(rule);
        XModelEntity e = getModelEntity();
        if(e != null) model.createRuleActionInh(getModelEntity(), rule);
    }
    
    public void removeRule(VRule rule) {
        model.removeRuleActionInh(getModelEntity(), rule);
        rules.remove(rule);
    }
    
    public void clearRules() {
        for (int i = 0; i < rules.size(); i++) {
            VRule rule = (VRule)rules.get(i);
            model.removeRuleActionInh(getModelEntity(), rule);
        }
        rules.clear();
    }
    
    public boolean isDescendant(String entity) {
    	return meta.getParentInfo().isDescendant(entity, entityName);
    }
    
    public XModelEntity getModelEntity() {
    	if(modelEntity == null) {
    		modelEntity = meta.getEntity(entityName);
    		if(modelEntity == null) {
    			VerificationPlugin.getPluginLog().logInfo("VModelImpl:Cannot find entity " + entityName);
    		}
    	}
        return modelEntity;
    }

    public boolean equals(Object o) {
        if (o instanceof VEntityImpl) {
            return ((VEntityImpl)o).entityName.equals(this.entityName);
        }
        return false;
    }
    
    public int hashCode() {
        return entityName.hashCode();
    }

}
