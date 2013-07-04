/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import java.util.Arrays;
import java.util.List;

import org.jboss.tools.usage.internal.reporting.JBossToolsComponents.IBundleProvider;

/**
 * @author Andre Dietisheim
 */
public class EclipseBundleProviderFake implements IBundleProvider {

	private List<String> bundleSymbolicNames;

	public EclipseBundleProviderFake(String... bundleSymbolicNames) {
		this.bundleSymbolicNames = Arrays.asList(bundleSymbolicNames);
	}

	public boolean isInstalled(String symbolicName) {
		return bundleSymbolicNames.contains(symbolicName);
	}
}