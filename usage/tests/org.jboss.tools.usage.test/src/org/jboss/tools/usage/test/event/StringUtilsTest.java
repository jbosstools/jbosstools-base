/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.event;

import static org.junit.Assert.assertEquals;

import org.jboss.tools.usage.util.StringUtils;
import org.junit.Test;

/**
 * @author Alexey Kazakov
 */
public class StringUtilsTest {

	@Test
	public void testCompressClassName() {
		assertCompressClassName("org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.ClassName",
								"org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.ClassName");

		assertCompressClassName("org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.LooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName",
								"org.xxx~.yyy~.zzz~.LooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName");

		assertCompressClassName("org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.LoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName",
								"org.xx~.yy~.zz~.LoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName");

		assertCompressClassName("org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.LooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName",
								"o~.x~.y~.z~.LooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName");

		assertCompressClassName("org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.LoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooongClassName",
								"org.xxxxxxxxxxx.yyyyyyyyyyy.zzzzzzzzzzz.Looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo~ame");
	}

	private void assertCompressClassName(String name, String expectedCompressedName) {
		String compressed = StringUtils.compressClassName(name);
		assertEquals(expectedCompressedName, compressed);
	}
}