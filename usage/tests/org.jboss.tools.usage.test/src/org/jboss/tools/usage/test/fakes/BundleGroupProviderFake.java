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
package org.jboss.tools.usage.test.fakes;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.osgi.framework.Bundle;

/**
 * @author Andre Dietisheim
 */
public class BundleGroupProviderFake implements IBundleGroupProvider {

	private String[] featureNames;

	public BundleGroupProviderFake(String... featureNames) {
		this.featureNames = featureNames;
	}

	public String getName() {
		throw new UnsupportedOperationException();
	}

	public IBundleGroup[] getBundleGroups() {
		IBundleGroup[] bundleGroups = new IBundleGroup[featureNames.length];
		for (int i = 0; i < featureNames.length; i++) {
			bundleGroups[i] = createBundleGroup(featureNames[i]);
		}
		return bundleGroups;
	}

	private IBundleGroup createBundleGroup(final String id) {
		return new IBundleGroup() {

			public String getVersion() {
				throw new UnsupportedOperationException();
			}

			public String getProviderName() {
				throw new UnsupportedOperationException();
			}

			public String getProperty(String key) {
				throw new UnsupportedOperationException();
			}

			public String getName() {
				throw new UnsupportedOperationException();
			}

			public String getIdentifier() {
				return id;
			}

			public String getDescription() {
				throw new UnsupportedOperationException();

			}

			public Bundle[] getBundles() {
				throw new UnsupportedOperationException();
			}
		};
	}

}