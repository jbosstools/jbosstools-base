/*
 * VRuleModel.java
 *
 * Created on July 14, 2003, 12:16 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.util.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.vrules.impl.VResultFactoryImpl;
import org.jboss.tools.common.verification.vrules.impl.VRuleImpl;
import org.jboss.tools.common.verification.vrules.layer.VModelFactory;
import org.jboss.tools.common.verification.vrules.layer.VModelImpl;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

/**
 *
 * @author  valera
 */
public class VRuleModel extends RegularObjectImpl implements PropertyChangeListener {
	private static final long serialVersionUID = 721113473428485561L;
	protected VRuleImpl rule;
    
    /** Creates a new instance of VRuleModel */
    public VRuleModel() {}
    
    public VRule getRule(VRuleSet ruleSet) {
        if (rule == null) {
            rule = new VRuleImpl();
            rule.setRuleSet(ruleSet);
            rule.setName(getAttributeValue("name"));
            rule.setDescription(getAttributeValue("description"));
            rule.setCategory(getAttributeValue("category"));
            rule.setEnabled(Boolean.valueOf(getAttributeValue("enabled")).booleanValue());
            rule.setResults(new VResult[0]);
            VEntity[] entities = getEntities(getAttributeValue("entities"));
            rule.setEntities(entities);
            String act = getAttributeValue("action");
            rule.setAction(getAction(act));
            try {
                rule.setSignificance(Integer.parseInt(getAttributeValue("significance")));
            } catch (NumberFormatException e) {
                rule.setSignificance(10);
            }
            VResultTemplate[] templates = getTemplates();
            VResultFactoryImpl factory = (VResultFactoryImpl)rule.getResultFactory();
            int sign = -1;
            for (int i = 0; i < templates.length; i++) {
                factory.addTemplate(templates[i]);
                sign = Math.max(sign, templates[i].getSignificance());
            }
            if (sign > 0) {
                rule.setSignificance(sign);
            }
            String p = getAttributeValue("properties");
            if(p != null && p.length() > 0) {
            	StringTokenizer st = new StringTokenizer(p, ",");
            	Properties properties = new Properties();
            	while(st.hasMoreTokens()) {
            		String t = st.nextToken();
            		int i = t.indexOf('=');
            		if(i < 0) continue;
            		String n = t.substring(0, i);
            		String v = t.substring(i + 1);
            		properties.setProperty(n, v);
            	}
            	if(properties.size() > 0) rule.setProperties(properties);
            }
            rule.addPropertyChangeListener(this);
        }
        return rule;
    }
    
    private VResultTemplate[] getTemplates() {
        XModelObject[] c = getChildren();
        VResultTemplate[] templates = new VResultTemplate[c.length];
        for (int i = 0; i < c.length; i++) {
            templates[i] = ((VResultTemplateModel)c[i]).getTemplate(rule);
        }
        return templates;
    }

    public VEntity[] getEntities(String list) {
        StringTokenizer tok = new StringTokenizer(list, ",");
        int count = tok.countTokens();
        VEntity[] entities = new VEntity[count];
		VModel model = VModelFactory.getModel(getModel());
        for (int i = 0; i < count; i++) {
            String entity = tok.nextToken().trim();
            entities[i] = model.getEntity(entity);
        }
        return entities;
    }
    
    public VAction getAction(String className) {
        if (className == null || className.length() == 0) return null;
        try {
            return (VAction)ModelFeatureFactory.getInstance().createFeatureInstance(className);
        } catch (Exception th) {
			if(VerificationPlugin.isDebugEnabled()) { 
				VerificationPlugin.getPluginLog().logError("Unable to create action \""+className+"\": " + "VRuleModel:getAction");
			}
        }
        return null;
    }
    
    public String setAttributeValue(String name, String value) {
        String result = super.setAttributeValue(name, value);
        if (rule != null) {
            if ("enabled".equals(name)) {
                rule.setEnabled(Boolean.valueOf(result).booleanValue());
            } else if ("name".equals(name)) {
                String oldName = rule.getName();
                rule.setName(result);
        		VModel vmodel = VModelFactory.getModel(getModel());
                ((VModelImpl)vmodel).updateRuleAction(rule, oldName);
            } else if ("action".equals(name)) {
                rule.setAction(getAction(result));
            } else if ("significance".equals(name)) {
                try {
                    rule.setSignificance(Integer.parseInt(result));
                } catch (NumberFormatException e) {}
            } else if ("entities".equals(name)) {
                VEntity[] oldEntities = rule.getEntities();
                VEntity[] newEntities = getEntities(result);
                rule.setEntities(newEntities);
                Map<String,VEntity> oldMap = new HashMap<String,VEntity>();
                for (int i = 0; i < oldEntities.length; i++) {
                    oldMap.put(oldEntities[i].getName(), oldEntities[i]);
                }
                for (int i = 0; i < newEntities.length; i++) {
                    if (oldMap.remove(newEntities[i].getName()) == null) {
                        newEntities[i].addRule(rule);
                    }
                }
                Iterator itr = oldMap.values().iterator();
                while (itr.hasNext()) {
                    VEntity entity = (VEntity)itr.next();
                    entity.removeRule(rule);
                }
            }
        }
        return result;
    }
    
    public boolean addChild(XModelObject child) {
        boolean res = super.addChild(child);
        if (res && rule != null && child instanceof VResultTemplateModel) {
            VResultTemplate template = ((VResultTemplateModel)child).getTemplate(rule);
            VResultFactoryImpl factory = (VResultFactoryImpl)rule.getResultFactory();
            factory.addTemplate(template);
        }
        return res;
    }
    
    public void removeChild(XModelObject child) {
        super.removeChild(child);
        if (rule != null && child instanceof VResultTemplateModel) {
            VResultTemplate template = ((VResultTemplateModel)child).getTemplate(rule);
            VResultFactoryImpl factory = (VResultFactoryImpl)rule.getResultFactory();
            factory.removeTemplate(template);
        }
    }
    
    public XModelObject[] getChildrenForSave() {
        // Do not save results
        return getChildren("VResultTemplate");
    } 

    // TODO: override
    protected Comparator<XModelObject> createComparator() {
        return super.createComparator();
    }

    // TODO: override
    public boolean isObjectEditable() {
        return super.isObjectEditable();
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
