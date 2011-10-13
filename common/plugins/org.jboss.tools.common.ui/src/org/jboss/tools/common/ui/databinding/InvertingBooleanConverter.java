/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.conversion.Converter;

/**
 * A converter that inverts a boolean. <code>true</code> gets <code>false</code>
 * and the opposite. If the converter gets a non-boolean value feeded in, it
 * will always return <code>false</code>.
 * 
 * @author André Dietisheim
 */
public class InvertingBooleanConverter extends Converter {

	public InvertingBooleanConverter() {
		super(Boolean.class, Boolean.class);
	}

	@Override
	public Object convert(Object fromObject) {
		if (!(fromObject instanceof Boolean)) {
			return Boolean.FALSE;
		}
		return !((Boolean) fromObject).booleanValue();
	}

}
