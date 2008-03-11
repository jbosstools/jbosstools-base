/*
 * VRulesTree.java
 *
 * Created on July 14, 2003, 4:05 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.trees.*;

/**
 *
 * @author  valera
 */
public class VRulesTree extends DefaultSiftedTree {
    
    private static boolean first = true;
    
    /** Creates a new instance of VRulesTree */
    public VRulesTree() {
    }
    
	public void dispose() {}

	public XModelObject getRoot() {
        XModelObject[] prjs = model.getRoot().getChildren("VManager");
        if (prjs.length == 0) return null;
        VManagerModel man = (VManagerModel)prjs[0];
        if (!man.developer && first) {
            // bad luck :)
            throw new RuntimeException("Members only");
        }
        first = false;
        return man;
    }
    
}
