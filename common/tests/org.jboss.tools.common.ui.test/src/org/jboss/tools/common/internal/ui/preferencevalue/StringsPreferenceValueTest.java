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
package org.jboss.tools.common.internal.ui.preferencevalue;

import static org.junit.Assert.assertEquals;

import org.jboss.tools.common.ui.preferencevalue.StringsPreferenceValue;
import org.junit.Before;
import org.junit.Test;

/**
 * @author André Dietisheim
 */
public class StringsPreferenceValueTest {

	private static final char DELIMITER = ',';

	private static class StringsPreferenceValueFake extends StringsPreferenceValue {

		private String values = "";

		public StringsPreferenceValueFake(char delimiter) {
			super(delimiter, null, null);
		}


		@Override
		protected void doStore(String value) {
			this.values = value;
		}

		@Override
		protected String doGet(String currentValue) {
			if (currentValue == null || currentValue.equals("")) {
				return values;
			} else {
				return currentValue;
			}
		}

	}

	private StringsPreferenceValue stringValues;

	@Before
	public void setUp() {
		this.stringValues = new StringsPreferenceValueFake(DELIMITER);
	}

	@Test
	public void canAddValues() {
		stringValues.add("11");
		stringValues.add("22");
		assertEquals(2, stringValues.get().length);
		stringValues.add("33");
		assertEquals(3, stringValues.get().length);
	}

	@Test
	public void identicalValueAreNotAdded() {
		String value = "11";
		stringValues.add(value);
		stringValues.add(value);
		assertEquals(1, stringValues.get().length);
	}

	@Test
	public void canAddDelimiterValue() {
		stringValues.add("11");
		assertEquals(1, stringValues.get().length);
		stringValues.add(DELIMITER + "");
		assertEquals(2, stringValues.get().length);
	}

	@Test
	public void delimiterValueIsReturnedCorrectly() {
		stringValues.add("11");
		assertEquals(1, stringValues.get().length);
		String delimiterValue = DELIMITER + "";
		stringValues.add(delimiterValue);
		String[] values = stringValues.get();
		assertEquals(2, values.length);
		values[1] = delimiterValue;
	}

}
