/*
 * VResultTemplateModel.java
 *
 * Created on July 29, 2003, 12:16 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.model.impl.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 *
 * @author  valera
 */
public class VResultTemplateModel extends RegularObjectImpl implements PropertyChangeListener {
	private static final long serialVersionUID = 7552672438471347238L;
    
    protected VResultTemplate template;
    
    /** Creates a new instance of VResultTemplateModel */
    public VResultTemplateModel() {
    }
    
    public VResultTemplate getTemplate(VRule rule) {
        if (template == null) {
            template = new VResultTemplate();
            template.setId(getAttributeValue("id"));
            template.setName(getAttributeValue("name"));
            template.setDescription(getAttributeValue("description"));
            template.setType(getAttributeValue("type"));
            try {
                template.setSignificance(Integer.parseInt(getAttributeValue("significance")));
            } catch (NumberFormatException e) {}
            template.setFormat(getFormat(getAttributeValue("message id"), rule));
            template.addPropertyChangeListener(this);
        }
        return template;
    }
    
    public VMessageFormat getFormat(String messageId, VRule rule) {
        return rule.getRuleSet().getMessageFormat(messageId);
    }

    public String setAttributeValue(String name, String value) {
        String result = super.setAttributeValue(name, value);
        if (template != null) {
            if ("id".equals(name)) {
                template.setId(result);
            } else if ("name".equals(name)) {
                template.setName(result);
            } else if ("description".equals(name)) {
                template.setDescription(result);
            } else if ("type".equals(name)) {
                template.setType(result);
            } else if ("significance".equals(name)) {
                try {
                    template.setSignificance(Integer.parseInt(result));
                } catch (NumberFormatException e) {}
            } else if ("message id".equals(name)) {
                template.setFormat(getFormat(result, ((VRuleModel)getParent()).getRule(null)));
            }
        }
        return result;
    }
    
    public String getPathPart() {
        return getAttributeValue("id");
    }

    public String getPresentationString() {
        return getAttributeValue("name");
    }
    
    public void propertyChange(PropertyChangeEvent evt) {
        String name = evt.getPropertyName();
        if ("id".equals(name) || "name".equals(name)
            || "description".equals(name) || "type".equals(name)
            || "significance".equals(name)) {
            String value = "" + evt.getNewValue();
            if (!value.equals(getAttributeValue(name))) {
                setAttributeValue(name, value);
                setModified(true);
            }
        } else if ("format".equals(name)) {
            VMessageFormat format = (VMessageFormat)evt.getNewValue();
            if (format != null) {
                String value = format.getId();
                if (!value.equals(getAttributeValue("message id"))) {
                    setAttributeValue("message id", value);
                    setModified(true);
                }
            }
        }
    }
    
}
