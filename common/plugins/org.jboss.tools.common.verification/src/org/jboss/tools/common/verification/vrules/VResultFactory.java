/*
 * VResultFactory.java
 *
 * Created on July 28, 2003, 11:21 AM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VResultFactory {
    
    /** Creates new VResult object.
     */
    public VResult getResult(String id, VObject sourceObject, Object sourcePosition,
            VObject targetObject, Object targetPosition, Object[] params);
        
    /** Returns template by id.
     */
    public VResultTemplate getTemplate(String id);
    
    /** Returns all templates.
     */
    public VResultTemplate[] getTemplates();
    
}
