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
package org.jboss.tools.common.model;

import org.jboss.tools.common.model.impl.XModelBufferImpl;

public class XModelTransferBuffer {
	private static XModelTransferBuffer instance = new XModelTransferBuffer();
	
	public static XModelTransferBuffer getInstance() {
		return instance;
	}
	
	private XModelTransferBuffer() {}

	private XModelBuffer buffer = null;
	
	public void enable() {
		if(buffer == null) {
			buffer = new XModelBufferImpl();
		}
	}
	
	public void disable() {
		buffer = null;
	}
	
	public boolean isEnabled() {
		return buffer != null;
	}
	
	public XModelBuffer getBuffer() {
		return buffer;
	}

}
