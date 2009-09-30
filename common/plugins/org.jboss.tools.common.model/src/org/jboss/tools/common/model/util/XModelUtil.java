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
package org.jboss.tools.common.model.util;

import java.util.Properties;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.*;

public class XModelUtil {
	private static final String PROPERTIES_ACTION = "Properties/Properties"; //$NON-NLS-1$
	private static final String EDIT_ACTION = "Edit"; //$NON-NLS-1$

	public static void openProperyDialog(XModelObject object){
		XActionInvoker.invoke(PROPERTIES_ACTION, object, new Properties());
	}
	
	public static void openEditor(XModelObject object){
		XActionInvoker.invoke(EDIT_ACTION, object, null);
	}
}

