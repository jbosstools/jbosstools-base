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
package org.jboss.tools.usage.test;

import static org.junit.Assert.*;

import org.jboss.tools.usage.tracker.internal.FocusPoint;
import org.jboss.tools.usage.tracker.internal.IFocusPoint;
import org.jboss.tools.usage.tracker.internal.JBossToolsFocusPoint;
import org.jboss.tools.usage.util.HttpEncodingUtils;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsFocusPointTest {

	private static final String JBOSS_TOOLS_VERSION = "42.0.42";

	private static final String ROOT = "root";

	private static final String CHILD = "child";

	private static final String URI_SEPARATOR_ENCODED = HttpEncodingUtils.checkedEncodeUtf8(FocusPoint.URI_SEPARATOR);

	private static final String TITLE_SEPARATOR_ENCODED = HttpEncodingUtils
			.checkedEncodeUtf8(FocusPoint.TITLE_SEPARATOR);

	@Test
	public void appendsJBossToolsVersionToTheEnd() throws Exception {
		IFocusPoint focusPoint = new JBossToolsFocusPointFake(ROOT)
				.setChild(new FocusPoint(CHILD)
						.setChild(new FocusPoint(CHILD)));
		String contentURI = focusPoint.getURI();

		assertNotNull(contentURI);
		assertEquals( URI_SEPARATOR_ENCODED
				+ ROOT
				+ URI_SEPARATOR_ENCODED
						+ CHILD + URI_SEPARATOR_ENCODED
						+ CHILD + URI_SEPARATOR_ENCODED
						+ JBOSS_TOOLS_VERSION, contentURI);

		String title = focusPoint.getTitle();
		assertNotNull(title);
		assertEquals(ROOT
				+ TITLE_SEPARATOR_ENCODED
						+ CHILD + TITLE_SEPARATOR_ENCODED
						+ CHILD + TITLE_SEPARATOR_ENCODED
						+ JBOSS_TOOLS_VERSION, title);
	}

	private static class JBossToolsFocusPointFake extends JBossToolsFocusPoint {

		public JBossToolsFocusPointFake(String name) {
			super(name);
		}

		@Override
		protected String getJBossToolsVersion() {
			return JBOSS_TOOLS_VERSION;
		}
	}
}
