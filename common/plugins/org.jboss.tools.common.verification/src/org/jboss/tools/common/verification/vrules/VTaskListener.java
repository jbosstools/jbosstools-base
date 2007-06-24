/*
 * VTaskListener.java
 *
 * Created on July 24, 2003, 4:23 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VTaskListener extends java.util.EventListener {

    /** Called by VTask when it started the job.
     */
    public void onStart();

    /** Called by VTask when rule has been applied to object.
     * @param rule applied rule
     * @param object verified object
     * @param results results of verification
     */
    public void onRuleApplied(VRule rule, VObject object, VResult[] results);
    
    /** Called by VTask when rule finishes its job.
     * @param rule finished rule.
     */
    public void onRuleFinished(VRule rule, VObject object);

    /** Called by VTask when it is about to pause.
     */
    public void onPause();

    /** Called by VTask when it resumed the job.
     */
    public void onResume();

    /** Called by VTask when it finishes processing.
     */
    public void onFinish();
}
