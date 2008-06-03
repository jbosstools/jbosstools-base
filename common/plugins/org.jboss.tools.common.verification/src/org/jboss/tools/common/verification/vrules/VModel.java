/*
 * VModel.java
 *
 * Created on July 11, 2003, 3:41 PM
 */

package org.jboss.tools.common.verification.vrules;

import org.eclipse.jdt.core.IType;

/**
 *
 * @author  valera
 */
public interface VModel {
    
    /** Returns object by path.
     */
    public VObject getObjectByPath(String path);
    
    /** Returns root objects.
     */
    public VObject[] getRootObjects();
    
    /** Returns entity by name
     */
    public VEntity getEntity(String name);
    
    /** Returns class loader
     */
///    public ClassLoader getClassLoader();
    
    /*
     * Returns object mapped to VManager
     */
    public Object getManagerKey();
    
    public IType getValidType(String className);

}
