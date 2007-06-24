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

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.*;

public class DnDUtil {
	
	public static XAction getEnabledAction(XModelObject object, XModelObject[] targets, String actionpath) {
		if(object == null) return null;
		XAction action = object.getModelEntity().getActionList().getAction(actionpath);
		if(action == null) return null;
		return (targets == null) 
				? ((!action.isEnabled(object)) ? null : action)
				: ((!action.isEnabled(object, targets)) ? null : action); 
	}
	
	public static boolean isCopyEnabled(XModelObject object, XModelObject[] targets) {
		return getEnabledCopyAction(object, targets) != null;
	}
	
	public static XAction getEnabledCopyAction(XModelObject object, XModelObject[] targets) {
		return getEnabledAction(object, targets, "CopyActions.Copy");
	}
	
	public static boolean copy(XModelObject object, XModelObject[] targets) {
		XAction copy = getEnabledCopyAction(object, targets);
		if(copy == null) return false;
		try {
			Properties p = new Properties();
			p.setProperty("isDrag", "true");
			if(targets == null) {
				copy.executeHandler(object, p);
			} else {
				copy.executeHandler(object, targets, p);
			}
			return true;
		} catch (Exception e) {
			return false;
		}
	}
	
	public static boolean isPasteEnabled(XModelObject object) {
		return getEnabledPasteAction(object) != null;
	}
	
	public static XAction getEnabledPasteAction(XModelObject object) {
		XAction action = getEnabledAction(object, null, "CopyActions.Paste");
		return (action != null) ? action : getEnabledAction(object, null, "MoveActions.Move");		
	}
	
	public static void paste(XModelObject object, Properties properties) throws Exception {
		XAction paste = getEnabledPasteAction(object);
		if(paste != null) paste.executeHandler(object, properties);
	}

}
