/*
 * VResultTemplateModel.java
 *
 * Created on July 29, 2003, 12:16 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.verification.vrules.VMessageFormat;
import org.jboss.tools.common.verification.vrules.VResultTemplate;
import org.jboss.tools.common.verification.vrules.VRule;

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
            template.setId(getAttributeValue("id")); //$NON-NLS-1$
            template.setName(getAttributeValue("name")); //$NON-NLS-1$
            template.setDescription(getAttributeValue("description")); //$NON-NLS-1$
            template.setType(getAttributeValue("type")); //$NON-NLS-1$
            String s = getAttributeValue("significance"); //$NON-NLS-1$
            try {
                template.setSignificance(Integer.parseInt(s));
            } catch (NumberFormatException e) {
            	//ignore
            }
            template.setFormat(getFormat(getAttributeValue("message id"), rule)); //$NON-NLS-1$
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
            if ("id".equals(name)) { //$NON-NLS-1$
                template.setId(result);
            } else if ("name".equals(name)) { //$NON-NLS-1$
                template.setName(result);
            } else if ("description".equals(name)) { //$NON-NLS-1$
                template.setDescription(result);
            } else if ("type".equals(name)) { //$NON-NLS-1$
                template.setType(result);
            } else if ("significance".equals(name)) { //$NON-NLS-1$
                try {
                    template.setSignificance(Integer.parseInt(result));
                } catch (NumberFormatException e) {
                	//ignore
                }
            } else if ("message id".equals(name)) { //$NON-NLS-1$
                template.setFormat(getFormat(result, ((VRuleModel)getParent()).getRule(null)));
            }
        }
        return result;
    }
    
    public String getPathPart() {
        return getAttributeValue("id"); //$NON-NLS-1$
    }

    public String getPresentationString() {
        return getAttributeValue("name"); //$NON-NLS-1$
    }
    
    public void propertyChange(PropertyChangeEvent evt) {
        String name = evt.getPropertyName();
        if ("id".equals(name) || "name".equals(name) //$NON-NLS-1$ //$NON-NLS-2$
            || "description".equals(name) || "type".equals(name) //$NON-NLS-1$ //$NON-NLS-2$
            || "significance".equals(name)) { //$NON-NLS-1$
            String value = "" + evt.getNewValue(); //$NON-NLS-1$
            if (!value.equals(getAttributeValue(name))) {
                setAttributeValue(name, value);
                setModified(true);
            }
        } else if ("format".equals(name)) { //$NON-NLS-1$
            VMessageFormat format = (VMessageFormat)evt.getNewValue();
            if (format != null) {
                String value = format.getId();
                if (!value.equals(getAttributeValue("message id"))) { //$NON-NLS-1$
                    setAttributeValue("message id", value); //$NON-NLS-1$
                    setModified(true);
                }
            }
        }
    }
    
}
