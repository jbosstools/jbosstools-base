/*
 * VManager.java
 *
 * Created on July 11, 2003, 3:36 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VManager {
    
    /** Returns model.
     */
    public VModel getModel();
    
    /** Returns all loaded rule sets.
     */
    public VRuleSet[] getRuleSets();
    
    /** Returns minimum significance for rules' results.
     */
    public int getMinSignificance();

    /** Sets minimum significance for rules' results.
     */
    public void setMinSignificance(int significance);

    /** Loads and activates given rule set.
     */
    public void loadRuleSet(VRuleSet ruleSet);
    
    /** Unloads and disactivates given rule set.
     */
    public void unloadRuleSet(VRuleSet ruleSet);
    
    /** Updates rule set.
     */
    public void updateRuleSet(VRuleSet ruleSet);
    
    /** Creates VTask that will verify given VObject using
     * current set of rules and significance settings.
     * Later modifications to rules will not affect created VTask.
     */
    public VTask createTask(VObject object);
    
    /** Creates VTask that will verify given VObject using
     * given set of rules.
     */
    public VTask createTask(VObject object, VRule[] rules);
    
    /** Returns message format to format VResult objects.
     */
    public VMessageFormat getMessageFormat();
}
