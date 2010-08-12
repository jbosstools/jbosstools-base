/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.jgoogleanalytics;

import org.jboss.tools.usage.util.EncodingUtils;

/**
 * Focus point of the application. It can represent data points like application
 * load, application module load, user actions, error events etc.
 */

public class FocusPoint {

	private String name;
	private FocusPoint childFocusPoint;
	public static final String URI_SEPARATOR = "/";
	public static final String TITLE_SEPARATOR = "-";

	public FocusPoint(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public FocusPoint setChild(FocusPoint childFocusPoint) {
		this.childFocusPoint = childFocusPoint;
		return this;
	}

	public FocusPoint getChild() {
		return childFocusPoint;
	}

	public String getContentURI() {
		StringBuilder builder = new StringBuilder();
		appendContentURI(builder, this);
		return EncodingUtils.checkedEncodeUtf8(builder.toString());
	}

	private void appendContentURI(StringBuilder builder, FocusPoint focusPoint) {
		FocusPoint parentFocuPoint = focusPoint.getChild();
		builder.append(URI_SEPARATOR);
		builder.append(focusPoint.getName());
		if (parentFocuPoint != null) {
			appendContentURI(builder, parentFocuPoint);
		}
	}

	public String getContentTitle() {
		StringBuilder builder = new StringBuilder();
		appendContentTitle(builder, this);
		return EncodingUtils.checkedEncodeUtf8(builder.toString());
	}

	private void appendContentTitle(StringBuilder builder, FocusPoint focusPoint) {
		FocusPoint childFocusPoint = focusPoint.getChild();
		builder.append(focusPoint.getName());
		builder.append(TITLE_SEPARATOR);
		if (childFocusPoint != null) {
			appendContentTitle(builder, childFocusPoint);
		}
	}
}
