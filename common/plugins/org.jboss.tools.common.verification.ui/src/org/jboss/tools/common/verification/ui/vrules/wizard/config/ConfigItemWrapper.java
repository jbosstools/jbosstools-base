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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import org.jboss.tools.common.verification.ui.vrules.wizard.*;

public abstract class ConfigItemWrapper implements TipSource {
	protected ConfigItemWrapper parent;
	protected boolean isSelectedInitial = false;
	public String toString() {
		return getPresentation();
	}
	public abstract String getPresentation();
	public abstract boolean isSelected();
	public abstract void cancel();
	public abstract void flip();
	public abstract void setDefaults();
	
	public ConfigItemWrapper getParent() {
		return parent;
	}
	
	public void setParent(ConfigItemWrapper parent) {
		this.parent = parent;
	}

	public boolean isEnabled() {
		ConfigItemWrapper p = getParent();
		while(p != null) {
			if(!p.isSelected()) return false;
			p = p.getParent();
		}
		return true;
	}

}
