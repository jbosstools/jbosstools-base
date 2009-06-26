/*
 * VResultModel.java
 *
 * Created on July 14, 2003, 11:52 AM
 */

package org.jboss.tools.common.verification.vrules.model;

import org.jboss.tools.common.model.impl.XModelObjectImpl;
import org.jboss.tools.common.verification.vrules.VResult;

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
        setAttributeValue("message", result.getMessage()); //$NON-NLS-1$
        setAttributeValue("significance", ""+result.getSignificance()); //$NON-NLS-1$ //$NON-NLS-2$
        setAttributeValue("source object", result.getSourceObject().getPath()); //$NON-NLS-1$
        setAttributeValue("source position", ""+result.getSourcePosition()); //$NON-NLS-1$ //$NON-NLS-2$
        setAttributeValue("target object", result.getTargetObject().getPath()); //$NON-NLS-1$
        setAttributeValue("target position", ""+result.getTargetPosition()); //$NON-NLS-1$ //$NON-NLS-2$
        setAttributeValue("type", result.getType()); //$NON-NLS-1$
    }
    
    public String getPathPart() {
        return ""+System.identityHashCode(this); //$NON-NLS-1$
    }

    public String getPresentationString() {
        String msg = getAttributeValue("message"); //$NON-NLS-1$
        return msg;
    }

    public String getMainIconName() {
        String type = getAttributeValue("type"); //$NON-NLS-1$
        if (type != null && type.length() > 0) {
            return "main.vrules.result-"+type; //$NON-NLS-1$
        } else {
            return super.getMainIconName();
        }
    }
}
