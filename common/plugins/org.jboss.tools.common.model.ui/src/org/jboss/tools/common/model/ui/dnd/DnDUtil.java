/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/


package org.jboss.tools.common.model.ui.dnd;


import java.util.Properties;

import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;


/**
 * The Class DnDUtil.
 */
public class DnDUtil {
    
    /**
     * Gets the enabled action.
     * 
     * @param actionpath the actionpath
     * @param object the object
     * @param targets the targets
     * 
     * @return the enabled action
     */
    public static XAction getEnabledAction(XModelObject object, XModelObject[] targets, String actionpath) {
        if (object == null)
            return null;
        XAction action = object.getModelEntity().getActionList().getAction(actionpath);
        if (action == null)
            return null;
        return (targets == null) ? ((!action.isEnabled(object)) ? null : action) : ((!action.isEnabled(object, targets)) ? null : action);
    }

    /**
     * Checks if is copy enabled.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return true, if is copy enabled
     */
    public static boolean isCopyEnabled(XModelObject object, XModelObject[] targets) {
        return getEnabledCopyAction(object, targets) != null;
    }

    /**
     * Gets the enabled copy action.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return the enabled copy action
     */
    public static XAction getEnabledCopyAction(XModelObject object, XModelObject[] targets) {
        return getEnabledAction(object, targets, "CopyActions.Copy"); //$NON-NLS-1$
    }

    /**
     * Copy.
     * 
     * @param object the object
     * @param targets the targets
     * 
     * @return true, if copy
     */
    public static boolean copy(XModelObject object, XModelObject[] targets) {
        XAction copy = getEnabledCopyAction(object, targets);
        if (copy == null)
            return false;
        try {
            Properties p = new Properties();
            p.setProperty("isDrag", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            if (targets == null) {
                copy.executeHandler(object, p);
            } else {
                copy.executeHandler(object, targets, p);
            }
            return true;
        } catch (XModelException e) {
            return false;
        }
    }

    /**
     * Checks if is paste enabled.
     * 
     * @param object the object
     * 
     * @return true, if is paste enabled
     */
    public static boolean isPasteEnabled(XModelObject object) {
        return getEnabledPasteAction(object) != null;
    }

    /**
     * Gets the enabled paste action.
     * 
     * @param object the object
     * 
     * @return the enabled paste action
     */
    public static XAction getEnabledPasteAction(XModelObject object) {
        XAction action = getEnabledAction(object, null, "CopyActions.Paste"); //$NON-NLS-1$
        return (action != null) ? action : getEnabledAction(object, null, "MoveActions.Move"); //$NON-NLS-1$
    }

    /**
     * Paste.
     * 
     * @param object the object
     * @param properties the properties
     * 
     * @throws XModelException the X model exception
     */
    public static void paste(XModelObject object, Properties properties) throws XModelException {
        XAction paste = getEnabledPasteAction(object);
        if (paste != null)
            paste.executeHandler(object, properties);
    }


}
