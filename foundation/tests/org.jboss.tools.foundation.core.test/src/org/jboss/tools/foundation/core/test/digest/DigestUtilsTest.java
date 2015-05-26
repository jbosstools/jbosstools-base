/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.foundation.core.test.digest;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.jboss.tools.foundation.core.digest.DigestUtils;
import org.junit.Test;

public class DigestUtilsTest {
	
	private static final String EXPECTED_SHA1 = "528FEA2AEA2AF98F4DB189399B6465923C7943EB".toLowerCase();
	
	@Test
	public void testSha1Path() throws IOException{
		Path zip = Paths.get("data", "sha1", "some.zip");
		String sha1 = DigestUtils.sha1(zip);
		assertEquals(EXPECTED_SHA1, sha1);
	}
	
	@Test
	public void testSha1File() throws IOException{
		File zip = new File("data", "sha1" + File.separator + "some.zip");
		String sha1 = DigestUtils.sha1(zip);
		assertEquals(EXPECTED_SHA1, sha1);
	}
	
	@Test
	public void testSha1Text() throws IOException{
		//See http://hipsum.co/
		String text = "Organic synth mixtape, Pitchfork High Life health goth Schlitz direct trade PBR&B irony normcore beard literally "
				+ "Thundercats post-ironic. Photo booth kale chips PBR selvage butcher. Wes Anderson Blue Bottle yr, gentrify High Life "
				+ "irony Vice keytar YOLO Intelligentsia cliche. Four dollar toast narwhal beard, ethical tousled meh blog Williamsburg "
				+ "keffiyeh drinking vinegar chillwave cred single-origin coffee. Cred occupy freegan, Tumblr shabby chic pug stumptown."
				+ " Chia next level kogi Wes Anderson. Thundercats beard bespoke distillery taxidermy.";
		
		String sha1 = DigestUtils.sha1(text);
		assertEquals("a75e6d4810e3edff6313d43fbc91bc294ef38af9", sha1);
	}
}
