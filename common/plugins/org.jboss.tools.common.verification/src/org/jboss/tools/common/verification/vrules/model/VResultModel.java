/*
 * VResultModel.java
 *
 * Created on July 14, 2003, 11:52 AM
 */

package org.jboss.tools.common.verification.vrules.model;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.model.impl.*;

/**
 *
 * @author  valera
 */
public class VResultModel extends XModelObjectImpl {
	private static final long serialVersionUID = 7259031343745755534L;
    
    protected VResult result;
    
    /** Creates a new instance of VResultModel */
    public VResultModel() {
    }
    
    public VResultModel(VResult result) {
        setResult(result);
    }
    
    public VResult getResult() {
        return result;
    }
    
    public void setResult(VResult result) {
        this.result = result;
        setAttributeValue("message", result.getMessage());
        setAttributeValue("significance", ""+result.getSignificance());
        setAttributeValue("source object", result.getSourceObject().getPath());
        setAttributeValue("source position", ""+result.getSourcePosition());
        setAttributeValue("target object", result.getTargetObject().getPath());
        setAttributeValue("target position", ""+result.getTargetPosition());
        setAttributeValue("type", result.getType());
    }
    
    public String getPathPart() {
        return ""+System.identityHashCode(this);
    }

    public String getPresentationString() {
        String msg = getAttributeValue("message");
        return msg;
    }

    public String getMainIconName() {
        String type = getAttributeValue("type");
        if (type != null && type.length() > 0) {
            return "main.vrules.result-"+type;
        } else {
            return super.getMainIconName();
        }
    }
}
