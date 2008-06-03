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

public class TestFaceletsJsfcResource extends TestDynamicResource {

	public final static String[] TAG_NAMES = {"h:inputText", "h:outputText", "h:dataTable"};

	public String getType() {
		return KbDinamicResource.FACELETS_JSFC_TYPE;
	}

	@Override
	public String[] getProposalLabels() {
		return TAG_NAMES;
	}
}