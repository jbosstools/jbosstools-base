/*
 * VAction.java
 *
 * Created on July 14, 2003, 6:36 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VAction {

    /** Executes this action against given object.
     * Returns all results associated with given object.
     */
    public VResult[] check(VObject object);
    
    /** Returns associated rule.
     */
    public VRule getRule();
    
    /** Associates this action with given rule.
     */
    public void setRule(VRule rule);
}
