/*
 * VRuleSet.java
 *
 * Created on July 11, 2003, 3:37 PM
 */

package org.jboss.tools.common.verification.vrules;

import java.util.ResourceBundle;

/**
 *
 * @author  valera
 */
public interface VRuleSet {
    
    /** Returns visible name of this rule set.
     */
    public String getName();
    
    /** Returns this rule set's vendor.
     */
    public String getVendor();
    
    /** Returns this rule set's version.
     * Recommended format XX[.XX][.XX] ...
     */
    public String getVersion();
    
    /** Returns this rule set's description.
     */
    public String getDescription();
    
    /** Returns URL that can be used to update this rule set.
     */
    public String getURL();
    
    public VRuleSet getParentRuleSet();
    
    /** Returns all rules within this rule set.
     */
    public VRule[] getRules();
    
	public VRuleSet[] getRuleSets();

    /** Returns true if this rule set enabled.
     */
    public boolean isEnabled();
    
    /** Enables or disables this rule set.
     */
    public void setEnabled(boolean enabled);
    
    /** Returns message format by id.
     */
    public VMessageFormat getMessageFormat(String id);
    
    /** Returns resource bundle associated with this rule set.
     */
    public ResourceBundle getResourceBundle();
}
