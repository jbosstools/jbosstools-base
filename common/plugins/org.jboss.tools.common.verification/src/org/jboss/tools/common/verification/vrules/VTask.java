/*
 * VTask.java
 *
 * Created on July 24, 2003, 4:28 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public interface VTask {
    
    /** Starts execution of rules asynchronously.
     * Resumes execution if it was paused.
     * This method returns immediately. To receive notifications
     * use addTaskListener(VTaskListener) method.
     */
    public void start();
    
    /** Pauses execution of rules.
     * Use start() method to resume execution.
     */
    public void pause();
    
    /** Stops execution of rules.
     */
    public void stop();
    
    /** Starts execution of rules synchronously.
     * This method returns after execution of all rules.
     */
    public void run();
    
    /** Adds a VTaskListener to the listener list.
     */
    public void addTaskListener(VTaskListener listener);
    
    /** Removes a VTaskListener from the listener list.
     */
    public void removeTaskListener(VTaskListener listener);
}
