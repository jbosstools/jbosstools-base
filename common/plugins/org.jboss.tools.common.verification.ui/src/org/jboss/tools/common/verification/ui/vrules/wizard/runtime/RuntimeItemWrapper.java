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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.ui.vrules.wizard.*;

public abstract class RuntimeItemWrapper implements TipSource {
	protected int status = 0;
	protected RuntimeItemWrapperListener listener;
	protected VManager manager;
	
	public void setManager(VManager manager) {
		this.manager = manager;
	}
	
	public void setListener(RuntimeItemWrapperListener listener) {
		this.listener = listener;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		if(this.status == status) return;
		this.status = status;
		listener.statusChanged();
	}

	public void mergeStatus(int status) {
		if(this.status < status) setStatus(status);
	}

	public String toString() {
		return getPresentation();
	}

	public abstract String getPresentation();

	public Object getObject() {
		return null;
	}

	public VRule[] getRules() {
		return new VRule[0];
	}

}
