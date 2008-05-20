/*
 * VRule.java
 *
 * Created on July 11, 2003, 3:37 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VRule {
    
    /** Returns visible name of this rule.
     */
    public String getName();

    /** Returns this rule's description.
     */
    public String getDescription();

    /** Returns category of this rule.
     * Used for filtering and ordering purposes.
     */
    public String getCategory();
    
    /** Returns all entities that this rule can be applied to.
     */
    public VEntity[] getEntities();
    
    /** Returns true if this rule enabled.
     */
    public boolean isEnabled();
    
    /** Enables or disables this rule.
     */
    public void setEnabled(boolean enabled);
    
    /** Returns all results of this rule.
     */
    public VResult[] getResults();
    
    /** Returns action associated with this rule.
     */
    public VAction getAction();
    
    /** Returns rule set.
     */
    public VRuleSet getRuleSet();
    
    /** Returns significance of this rule from 0 to 10.
     */
    public int getSignificance();
    
    /** Returns result factory.
     */
    public VResultFactory getResultFactory();
    
    /**
     * Returns property.
     * For example, properties may be used to set parameters 
     * to VAction implementation
     */    
	public String getProperty(String name);

}

