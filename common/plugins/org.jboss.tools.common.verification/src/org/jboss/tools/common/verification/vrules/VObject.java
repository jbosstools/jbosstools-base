/*
 * VObject.java
 *
 * Created on July 11, 2003, 3:41 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VObject {
    
    /** Returns this object's entity.
     */
    public VEntity getEntity();
    
    /** Returns path to this object.
     */
    public String getPath();
    
    /** Returns this object's children.
     */
    public VObject[] getChildren();
    
    /** Returns child of this object by relative path.
     */
    public VObject getChild(String path);
    
    /** Returns parent of this object.
     */
    public VObject getParent();
    
    /** Returns model.
     */
    public VModel getModel();
    
    /** Returns attribute by name.
     */
    public Object getAttribute(String name);
    
    /** Returns last modification time of this object.
     */
    public long getTimeStamp();
}
