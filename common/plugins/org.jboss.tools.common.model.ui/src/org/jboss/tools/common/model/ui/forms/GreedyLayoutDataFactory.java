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

import org.eclipse.swt.layout.GridData;

public class GreedyLayoutDataFactory implements ILayoutDataFactory {

	private static GreedyLayoutDataFactory INSTANCE = new GreedyLayoutDataFactory();

	private GreedyLayoutDataFactory() {
	}

	public static GreedyLayoutDataFactory getInstance() {
		return INSTANCE;
	}

    public Object createLayoutData(AttributeControlType type) {
    	if(type == AttributeControlType.LABEL) {
			GridData gd = new GridData();
			gd.verticalAlignment = GridData.BEGINNING;
    		return gd;
    	} else if(type == AttributeControlType.EDITOR) {
			return new GridData(GridData.FILL_BOTH);
		}
        throw new RuntimeException("Attribute control type may be only Label or Editor but this is " + type);
    }
}