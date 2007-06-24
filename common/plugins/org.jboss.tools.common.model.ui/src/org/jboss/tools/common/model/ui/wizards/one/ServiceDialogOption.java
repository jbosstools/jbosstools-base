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
package org.jboss.tools.common.model.ui.wizards.one;

import java.util.Properties;
import org.jboss.tools.common.model.ServiceDialog;

public class ServiceDialogOption {
	protected String text;
	protected String property;
	
	public ServiceDialogOption(String text) {
		this.text = text;
	}
	
	public boolean register(Properties p, int k) {
		String message = ServiceDialog.CHECKBOX_MESSAGE;
		property = ServiceDialog.CHECKED;
		if(k > 0) {
			property += "_" + k;
			message += "_" + k;
		}
		p.setProperty(message, text);
		p.put(property, Boolean.FALSE);
		return true;
	}
	
	public boolean isSelected(Properties p) {
		if(property == null) return false;
		return Boolean.TRUE.equals(p.get(property));
	}
	
	public void run() {
	}

}
