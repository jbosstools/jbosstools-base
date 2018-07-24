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
package org.jboss.tools.common.launcher.core.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.Collection;

import org.jboss.tools.common.launcher.core.model.Booster;
import org.jboss.tools.common.launcher.core.model.Catalog;
import org.jboss.tools.common.launcher.core.model.Mission;
import org.jboss.tools.common.launcher.core.model.Version;
import org.junit.BeforeClass;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class CatalogTest {
	private static ObjectMapper mapper;
	
	@BeforeClass
	public static void setup() {
		mapper = new ObjectMapper();
	}
	
	@Test
	public void testBoosters() throws JsonParseException, JsonMappingException, IOException {
		Catalog catalog = mapper.readValue(CatalogTest.class.getResourceAsStream("/catalog.json"), Catalog.class);
		assertNotNull(catalog);
		assertNotNull(catalog.getBoosters());
		assertEquals(98, catalog.getBoosters().size());
		Booster booster = catalog.getBoosters().iterator().next();
		assertNotNull(booster);
		assertEquals("istio-circuit-breaker", booster.getMission());
		assertEquals("Eclipse Vert.x - Istio - Circuit Breaker", booster.getName());
		assertEquals("Runs a Eclipse Vert.x application that utilizes Circuit Breakers on Istio", booster.getDescription());
		assertEquals("vert.x", booster.getRuntime());
		assertEquals("community", booster.getVersion());
	}
	
	@Test
	public void testRuntimes() throws JsonParseException, JsonMappingException, IOException {
		Catalog catalog = mapper.readValue(CatalogTest.class.getResourceAsStream("/catalog.json"), Catalog.class);
		assertNotNull(catalog);
		assertNotNull(catalog.getRuntimes());
		assertEquals(6, catalog.getRuntimes().size());
		org.jboss.tools.common.launcher.core.model.Runtime runtime = catalog.getRuntimes().iterator().next();
		assertNotNull(runtime);
		assertEquals("vert.x", runtime.getId());
		assertEquals("Eclipse Vert.x", runtime.getName());
		assertEquals("A tool-kit for building reactive applications on the JVM.", runtime.getDescription());
		Collection<Version> versions = runtime.getVersions();
		assertNotNull(versions);
		assertEquals(2, versions.size());
		Version version = versions.iterator().next();
		assertNotNull(version);
		assertEquals("redhat", version.getId());
		assertEquals("3.5.1.redhat-004 (RHOAR)", version.getName());
	}

	@Test
	public void testMissions() throws JsonParseException, JsonMappingException, IOException {
		Catalog catalog = mapper.readValue(CatalogTest.class.getResourceAsStream("/catalog.json"), Catalog.class);
		assertNotNull(catalog);
		assertNotNull(catalog.getMissions());
		assertEquals(11, catalog.getMissions().size());
		Mission mission = catalog.getMissions().iterator().next();
		assertNotNull(mission);
		assertEquals("crud", mission.getId());
		assertEquals("CRUD", mission.getName());
		assertEquals("Expand on the REST API Level 0 to perform CRUD operations on a PostgreSQL database using a simple HTTP REST API", mission.getDescription());
	}
}
