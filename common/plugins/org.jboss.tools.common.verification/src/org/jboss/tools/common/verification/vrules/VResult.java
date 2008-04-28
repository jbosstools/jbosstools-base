/*
 * VResult.java
 *
 * Created on July 11, 2003, 3:38 PM
 */

package org.jboss.tools.common.verification.vrules;

/**
 *
 * @author  valera
 */
public class VResult {
    
    public static final String TYPE_ERROR  = "error";
    public static final String TYPE_WARN   = "warning";
    public static final String TYPE_INFO   = "info";
    public static final String TYPE_HIDDEN = "hidden";
    
    public static final int SIGN_ERROR  = 10;
    public static final int SIGN_WARN   = 5;
    public static final int SIGN_INFO   = 2;
    public static final int SIGN_HIDDEN = 0;

    protected String message;
    protected int significance;
    protected VObject sourceObject;
    protected VObject targetObject;
    protected Object sourcePosition;
    protected Object targetPosition;
    protected long timeStamp;
    protected String type;

    /** Creates a new instance of VResult */
    public VResult() {
        timeStamp = System.currentTimeMillis();
    }
    
    /** Returns type of this result - error, warning etc.
     */
    public String getType() {
        return type;
    }
    
    /** Sets type of this result - error, warning etc.
     */
    public void setType(String type) {
        this.type = type;
    }
    
    /** Returns significance of this result from 0 to 10.
     * Used to filter out unsignificant results.
     */
    public int getSignificance() {
        return significance;
    }
    
    /** Sets significance of this result.
     * Used to filter out unsignificant results.
     */
    public void setSignificance(int significance) {
        if (significance > 10) {
            this.significance = 10;
        } else if (significance < 0) {
            this.significance = 0;
        } else {
            this.significance = significance;
        }
    }
    
    /** Returns visible message. Message can contain HTML tags
     * if supported by visual components.
     */
    public String getMessage() {
        return message;
    }
    
    /** Sets visible message. Message can contain HTML tags
     * if supported by visual components.
     */
    public void setMessage(String message) {
        this.message = message;
    }
    
    
    /** Returns source object - object that triggered
     * creation of this result.
     */
    public VObject getSourceObject() {
        return sourceObject;
    }
    
    /** Sets source object - object that triggered
     * creation of this result.
     */
    public void setSourceObject(VObject sourceObject) {
        this.sourceObject = sourceObject;
    }
    
    /** Returns position within source object.
     * Ex: line number for text files.
     */
    public Object getSourcePosition() {
        return sourcePosition;
    }
    
    /** Sets position within source object.
     * Ex: line number for text files.
     */
    public void setSourcePosition(Object sourcePosition) {
        this.sourcePosition = sourcePosition;
    }
    
    /** Returns target object - object, which state is inconsistent
     * with source object. Can be the same as source object for internal
     * inconsistencies.
     */
    public VObject getTargetObject() {
        return targetObject;
    }
    
    /** Sets target object - object, which state is inconsistent
     * with source object. Can be the same as source object for internal
     * inconsistencies.
     */
    public void setTargetObject(VObject targetObject) {
        this.targetObject = targetObject;
    }
    
    /** Returns position within target object.
     * Ex: line number for text files.
     */
    public Object getTargetPosition() {
        return targetPosition;
    }
    
    /** Sets position within target object.
     * Ex: line number for text files.
     */
    public void setTargetPosition(Object targetPosition) {
        this.targetPosition = targetPosition;
    }
    
    /** Returns time when this result has been created.
     * Can be used to skip unchanged objects.
     */
    public long getTimeStamp() {
        return timeStamp;
    }
    
    /** Sets time when this result has been created.
     * Can be used to skip unchanged objects.
     */
    public void setTimeStamp(long timeStamp) {
        this.timeStamp = timeStamp;
    }

}
