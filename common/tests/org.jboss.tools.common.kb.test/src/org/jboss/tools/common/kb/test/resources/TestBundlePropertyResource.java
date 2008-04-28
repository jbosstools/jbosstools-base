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
package org.jboss.tools.common.kb.test.resources;

import org.jboss.tools.common.kb.KbDinamicResource;

public class TestBundlePropertyResource extends TestDynamicResource {

	public final static String[] PROPERY_NAMES = {"bundle_property1", "bundle_property2", "bundle_property3"};

	public String getType() {
		return KbDinamicResource.BUNDLE_PROPERTY_TYPE;
	}

	@Override
	public String[] getProposalLabels() {
		return PROPERY_NAMES;
	}
}