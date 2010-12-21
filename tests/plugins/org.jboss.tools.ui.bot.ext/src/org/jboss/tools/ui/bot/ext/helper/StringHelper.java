/*******************************************************************************
 * Copyright (c) 2007-2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.helper;

import static org.junit.Assert.fail;
import org.eclipse.swt.graphics.Point;

/**
 * Class provides api for search position in text editor, designed for usage with swtbot
 * 
 * @author jpeterka
 * 
 */
public class StringHelper {
	String text;

	/**
	 * Default constructor
	 * 
	 * @param text
	 *            - given tex editor text
	 */
	public StringHelper(String text) {
		this.text = text;
	}

	/**
	 * Returns position before string fragment in the text
	 * 
	 * @param fragment
	 *            text fragment
	 * @return position
	 */
	public Point getPositionBefore(String fragment) {
		return getPosition(fragment);
	}

	/**
	 * Returns position after string fragment in the text
	 * 
	 * @param fragment
	 *            text fragment
	 * @return position
	 */
	public Point getPositionAfter(String fragment) {
		Point p = getPosition(fragment);
		return new Point(p.x + fragment.length() - 1, p.y);
	}

	private Point getPosition(String fragment) {
		Point p = new Point(0, 0);

		int index = text.indexOf(fragment);
		if (index != -1) {
			p = getRowCount(index);
		} else
			fail("Can't find required fragment: " + fragment);

		return p;
	}

	private Point getRowCount(int lastIndex) {
		int rowCount = 0;
		boolean finished = false;
		int colIndex = 0;
		int prevLinePos = 0;
		int newLinePos = 0;

		while (!finished) {

			newLinePos = text.indexOf('\n', newLinePos);
			if ((newLinePos != -1) && (newLinePos < lastIndex)) {
				rowCount++;
				prevLinePos = newLinePos;
				newLinePos = newLinePos + 1;
			} else {
				if (newLinePos == -1) {
					colIndex = text.length() - lastIndex;
				} else {
					colIndex = lastIndex - prevLinePos - 1;
				}
				finished = true;
			}
		}

		return new Point(colIndex, rowCount);
	}

	/** Debug only **/
	public static void main(String[] args) {
		String s = "<html>\n     <body>\nHello World\n     </body>\n</html>";
		StringHelper helper = new StringHelper(s);
		String search = "<body>";
		Point p = helper.getPositionAfter(search);
		System.out.println(s);
		System.out.println("Searching for..." + search);
		System.out.println(p.toString());
		search = "boasdfdy>";
		System.out.println("Searching for..." + search);
		p = helper.getPositionAfter(search);
		System.out.println(p.toString());
	}
}
