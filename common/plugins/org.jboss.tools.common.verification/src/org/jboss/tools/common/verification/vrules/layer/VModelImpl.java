/*
 * VModelImpl.java
 *
 * Created on July 11, 2003, 6:48 PM
 */

package org.jboss.tools.common.verification.vrules.layer;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import java.util.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IType;

/**
 *
 * @author  valera
 */
public class VModelImpl implements VModel {
    private XModel model;
    XModelMetaData meta;
    private IProject project;
    private Map<String,VEntity> entities;
    
    /** Creates a new instance of VModelImpl */
    public VModelImpl(XModel model) {
        this.model = model;
        meta = model.getMetaData();
        project = (IProject)model.getProperties().get("project");
        this.entities = new HashMap<String,VEntity>();
    }
    
    public boolean isMain() {
    	return project == null;
    }
    
	public Object getManagerKey() {
		return model;
	}
	
    public VEntity getEntity(String name) {
    	VEntity entity = (VEntity)entities.get(name);
    	if(entity == null) {
    		entity = new VEntityImpl(meta, name, this);
    		entities.put(name, entity);
    	}
        return entity;
    }
    
    public VObject getObjectByPath(String path) {
        XModelObject o = model.getByPath(path);
        return o == null ? null : new VObjectImpl(o, this);
    }
    
    public VObject[] getRootObjects() {
        return new VObject[] { new VObjectImpl(model.getRoot(), this) };
    }
    
    private static XActionItem.Acceptor acceptor1 = new XActionItem.Acceptor() {
        public boolean accepts(XActionItem item) {
            return item.getName().charAt(0) != '[';
        }
    };
    
    public XActionListImpl createGlobalActionList(XModelEntity entity) {
        XActionList global = (XActionList)model.getMetaData().getGlobalActions().getItem("VerifyActions");
        XActionListImpl entityActions = (XActionListImpl)entity.getActionList();
        XActionListImpl verifyActions = (XActionListImpl)entityActions.getItem("VerifyActions");
        if (verifyActions == null) {
            verifyActions = (XActionListImpl)global.copy(acceptor1);
            entityActions.addActionItem(verifyActions);
        }
        return verifyActions;
    }
    
    public XActionListImpl getGlobalActionList(XModelEntity entity) {
        XActionListImpl verifyActions = (XActionListImpl)entity.getActionList().getItem("VerifyActions");
        return verifyActions;
    }
    
//    public void removeGlobalActionList(XModelEntity entity) {
//    	if(!isMain()) return;
//        XActionListImpl entityActions = (XActionListImpl)entity.getActionList();
//        XActionListImpl verifyActions = (XActionListImpl)entityActions.getItem("VerifyActions");
//        if (verifyActions != null) {
//            removeAction(entityActions, verifyActions.getName());
//        }
//    }
    
    public XActionListImpl createRuleSetActionList(XModelEntity entity, VRuleSet ruleSet) {
//        XActionList global = (XActionList)model.getMetaData().getGlobalActions().getItem("VerifyActions");
//        XActionListImpl verifyActions = 
        	createGlobalActionList(entity);
		return null;        
    }
    
    public void createRuleAction(XModelEntity entity, VRule rule) {
//        XActionListImpl ruleSetList = 
        	createRuleSetActionList(entity, rule.getRuleSet());
    }
    
    public void createRuleActionInh(XModelEntity entity, VRule rule) {
        createRuleAction(entity, rule);
        Iterator ancestors = getAncestors(entity.getName()).iterator();
        while(ancestors.hasNext()) {
        	String n = ancestors.next().toString();
        	XModelEntity ent = model.getMetaData().getEntity(n);
            if(ent != null) createRuleAction(ent, rule);
        }
    }
    
//    private void removeAction(XActionList list, String action) {
//    }

    private Set getAncestors(String name) {
    	return model.getMetaData().getParentInfo().getAncestors(name);
    }

    public XActionListImpl getRuleSetActionList(XModelEntity entity, String ruleSetName) {
        XActionListImpl verifyActions = getGlobalActionList(entity);
        if (verifyActions == null) return null;
        
        XActionListImpl dynamicList = (XActionListImpl)verifyActions.getItem("DynamicActions");
        XActionListImpl ruleSetList = (XActionListImpl)dynamicList.getItem(ruleSetName);
        return ruleSetList;
    }

    public void updateRuleSetActionList(VRuleSet ruleSet, String oldName) {
    }
    
    public void removeRuleSetActionList(VRuleSet ruleSet) {
//		if(!isMain()) return;
    }
    
    public void removeRuleSetActionList(XModelEntity entity, VRuleSet ruleSet) {
//		if(!isMain()) return;
    }
    
    public XActionImpl getRuleAction(XModelEntity entity, String ruleSetName, String ruleName) {
        XActionListImpl ruleSetList = getRuleSetActionList(entity, ruleSetName);
        if (ruleSetList == null) return null;
        
        XActionListImpl dynamicList2 = (XActionListImpl)ruleSetList.getItem("DynamicActions");
        XActionImpl ruleAction = (XActionImpl)dynamicList2.getItem(ruleName);
        return ruleAction;
    }
    
    public void updateRuleAction(VRule rule, String oldName) {
//		if(!isMain()) return;
    }
    
    public void removeRuleAction(VRule rule) {
//		if(!isMain()) return;
    }
    
    public void removeRuleAction(XModelEntity entity, VRule rule) {
//		if(!isMain()) return;
    }
    
    public void removeRuleActionInh(XModelEntity entity, VRule rule) {
//    	if(!isMain()) return;
    }
    
	public IType getValidType(String className) {
		if(project == null) return null;
		IType type = EclipseResourceUtil.getValidType(project, className);
		if(type != null) return type;
		if(EclipseResourceUtil.isContainedInOutput(project, className)) {
			// Eclipse does not have type in this case, 
			// so we return something instead of null 
			// This is ok while result is only compared to null
			return EclipseResourceUtil.getValidType(project, "java.lang.Class");
		}
		return null; 
	}
    
}
