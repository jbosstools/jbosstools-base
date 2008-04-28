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
package org.jboss.tools.common.model.ui.forms;

import java.io.ObjectStreamException;
import java.io.Serializable;

/**
 * @author Igels
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class AttributeControlType implements Serializable {

	private static int nextOrdinal = 0;

	private final int ordinal = nextOrdinal++;

	private final String type;

	private AttributeControlType(String type) {
		this.type = type;
	}
	
	public String toString() {
		return type;
	}

	public static final AttributeControlType LABEL = new AttributeControlType("Label");

	public static final AttributeControlType EDITOR = new AttributeControlType("Editor");

	private static final AttributeControlType[] PRIVATE_VALUES = {LABEL, EDITOR};

	private Object readResolve() throws ObjectStreamException {
		return PRIVATE_VALUES[ordinal];
	}
}