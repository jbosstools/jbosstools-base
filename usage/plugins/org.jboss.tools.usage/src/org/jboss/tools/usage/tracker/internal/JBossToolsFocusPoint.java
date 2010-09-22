/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.tracker.internal;

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.HttpEncodingUtils;

/**
 * A focus point that always reports the current jboss tools version as last
 * component.
 */
public class JBossToolsFocusPoint extends FocusPoint {

	public JBossToolsFocusPoint(String name) {
		super(name);
	}

	@Override
	public String getURI() {
		StringBuilder builder = new StringBuilder();
		appendContentURI(builder, this);
		appendJBossToolsVersion(builder, URI_SEPARATOR);
		return HttpEncodingUtils.checkedEncodeUtf8(builder.toString());
	}

	protected void appendJBossToolsVersion(StringBuilder builder, String separator) {
		builder.append(separator);
		builder.append(getJBossToolsVersion());
	}

	@Override
	public String getTitle() {
		StringBuilder builder = new StringBuilder();
		appendContentTitle(builder, this);
		appendJBossToolsVersion(builder, TITLE_SEPARATOR);
		return HttpEncodingUtils.checkedEncodeUtf8(builder.toString());
	}
	
	protected String getJBossToolsVersion() {
		return JBossToolsUsageActivator.getDefault().getBundle().getVersion().toString();
	}
}
