/*
 * VEntity.java
 *
 * Created on July 11, 2003, 3:41 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VEntity {
    
    /** Returns entity name.
     */
    public String getName();
    
    /** Returns entities of objects that can be children of
     * objects of this entity.
     */
    public VEntity[] getChildren();
    
    /** Returns true if objects of given entity can be descendants
     * of objects of this entity.
     */
    public boolean isDescendant(String entity);
    
    /** Returns rules associated with this entity.
     */
    public VRule[] getRules();
    
    /** Associates given rule with this entity.
     */
    public void addRule(VRule rule);
    
    /** Disassociates given rule from this entity.
     */
    public void removeRule(VRule rule);
    
    /** Disassociates all rules from this entity.
     */
    public void clearRules();
    
}
