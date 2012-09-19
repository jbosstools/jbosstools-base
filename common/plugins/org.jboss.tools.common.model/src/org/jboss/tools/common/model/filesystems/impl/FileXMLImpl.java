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
package org.jboss.tools.common.model.filesystems.impl;

import org.jboss.tools.common.model.filesystems.impl.AbstractXMLFileImpl;

public class FileXMLImpl extends AbstractXMLFileImpl {
	private static final long serialVersionUID = 1L;
	boolean markersReset = false;
	
	public FileXMLImpl() {}

	public boolean isIncorrect() {
		return errors.length > 0;
	}
	
	public boolean hasChildren() {
		return false;
	}

	public String get(String name) {
		if(name.equals("_hasErrors_")) { //$NON-NLS-1$
			return super.get(ATTR_NAME_IS_INCORRECT);
		}
		if(!markersReset && isActive()
				&& !"NAME".equals(name) && !"EXTENSION".equals(name) && !"overlapped".equals(name)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			updateMarkers(super.get(ATTR_NAME_BODY));
		}
		return super.get(name);
	}	

	public void set(String name, String value) {
		boolean isBody = ATTR_NAME_BODY.equals(name) || ATTR_NAME__BODY_.equals(name);
		if(isActive() && isBody && !value.equals(get(name))) {
			super.set(name, value);
			updateMarkers(value);
		} else {
			super.set(name, value);
		}
	}
	
	private void updateMarkers(String body) {
		markersReset = true;
		if(!isOverlapped()) {
			setErrors(body, false, false);
		}
	}
	
}
