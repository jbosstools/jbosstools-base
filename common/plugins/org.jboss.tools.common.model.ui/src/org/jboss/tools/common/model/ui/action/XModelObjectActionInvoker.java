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
package org.jboss.tools.common.model.ui.action;

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.eclipse.swt.widgets.*;

public class XModelObjectActionInvoker implements SpecialWizard {
	private Object data;

	public XModelObjectActionInvoker() {}

	public void setObject(Object object) {
		data = object;
	}

	public int execute() {
		Object[] os = (Object[])data;
		data = null;
		XAction action = (XAction)os[0];
		XModelObject o = (XModelObject)os[1];
		Properties p = (Properties)os[2];
		XModelObject[] targets = (os.length < 4) ? null : (XModelObject[])os[3];
		XModelObjectAction mo = new XModelObjectAction(action, o, targets, new Object[]{p});
		if(p != null && p.get("shell") != null) mo.setShell((Shell)p.get("shell"));
		mo.actionPerformed();
		return 0;
	}
}
