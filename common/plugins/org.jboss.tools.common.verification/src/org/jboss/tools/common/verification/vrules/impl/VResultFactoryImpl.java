/*
 * VResultFactoryImpl.java
 *
 * Created on July 28, 2003, 11:27 AM
 */

package org.jboss.tools.common.verification.vrules.impl;

import org.jboss.tools.common.verification.vrules.*;
import java.util.*;

/**
 *
 * @author  valera
 */
public class VResultFactoryImpl implements VResultFactory {
    
    private Map<String,VResultTemplate> templates = new HashMap<String,VResultTemplate>();
    
    /** Creates a new instance of VResultFactoryImpl */
    public VResultFactoryImpl() {
    }
    
    public VResult getResult(String id, VObject sourceObject, Object sourcePosition, VObject targetObject, Object targetPosition, Object[] params) {
        VResultTemplate template = getTemplate(id);
        if (template == null) return null;
        return template.getResult(sourceObject, sourcePosition, targetObject, targetPosition, params);
    }
    
    public VResultTemplate getTemplate(String id) {
        return (VResultTemplate)templates.get(id);
    }
    
    public VResultTemplate[] getTemplates() {
        return (VResultTemplate[])templates.values().toArray(new VResultTemplate[templates.size()]);
    }
    
    public void removeTemplate(VResultTemplate template) {
        templates.remove(template.getId());
    }
    
    public void addTemplate(VResultTemplate template) {
        templates.put(template.getId(), template);
    }
}
