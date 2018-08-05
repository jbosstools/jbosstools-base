/*************************************************************************************
 * Copyright (c) 2018 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.launcher.ui.wizard.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.jboss.tools.common.launcher.core.model.Catalog;
import org.jboss.tools.common.launcher.ui.wizard.NewLauncherProjectModel;
import org.junit.BeforeClass;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class NewLauncherProjectWizardModelTest {
	private static ObjectMapper mapper;
	
	@BeforeClass
	public static void setup() {
		mapper = new ObjectMapper();
	}

	@Test
	public void testDefaultModel()  {
		NewLauncherProjectModel model = new NewLauncherProjectModel();
		assertNotNull(model.getArtifactId());
		assertNotNull(model.getGroupId());
		assertNotNull(model.getVersion());
		assertNotNull(model.getLocation());
		assertTrue(model.isUseDefaultLocation());
	}
	
	@Test
	public void testModelWithCatalog() throws JsonParseException, JsonMappingException, IOException  {
		NewLauncherProjectModel model = new NewLauncherProjectModel();
		Catalog catalog = mapper.readValue(NewLauncherProjectWizardModelTest.class.getResourceAsStream("/catalog.json"), Catalog.class);
		model.setCatalog(catalog);
		assertNotNull(model.getSelectedMission());
		assertNotNull(model.getSelectedBooster());
	}

	
}
