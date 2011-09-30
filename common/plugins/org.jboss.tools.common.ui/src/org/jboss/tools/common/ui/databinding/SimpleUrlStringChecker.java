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

import java.util.regex.Pattern;

/**
 * @author André Dietisheim
 */
public class SimpleUrlStringChecker {

	private static final Pattern simpleUrlPattern = Pattern.compile(".+://[^\\.]+\\.[^\\.]+(\\.[^\\.]+){0,1}");
	private String url;

	public SimpleUrlStringChecker(String url) {
		this.url = url;
	}

	public boolean isValid() {
		return simpleUrlPattern.matcher(url).matches();
	}
	
}
