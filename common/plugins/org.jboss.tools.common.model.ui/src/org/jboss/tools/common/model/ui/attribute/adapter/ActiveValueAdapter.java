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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.ui.*;
import org.eclipse.swt.widgets.*;

public class ActiveValueAdapter extends DefaultValueAdapter implements IActionHelper {
	protected String qActionName = null;
	protected String command = null;
	protected SpecialWizardSupport support;
	
	public void dispose() {
		super.dispose();
		support = null;
	}

	public void setAttribute(XAttribute attribute) {
		super.setAttribute(attribute);
		support = (SpecialWizardSupport)attribute.getEditor().getContext();
		qActionName = "...:" + attribute.getName();
		Properties p = support.getProperties();
		String comd = (p == null) ? null : p.getProperty(qActionName);
		if(comd == null) comd = "...";
		if(!comd.equals(command)) command = comd;
	}
	
	public String getCommand() {
		return command;	
	}
	
	public String invoke(Control control) {
		support.fireCommand(qActionName);
		load();
		return "" + getValue();
	}
	
	public Object getAdapter(Class adapter) {
		if (adapter == IActionHelper.class) return this;
		return super.getAdapter(adapter);
	}
	
}
