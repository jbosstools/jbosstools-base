/*******************************************************************************
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.HttpURLConnection;

import org.jboss.tools.usage.http.HttpGetRequest;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;
import org.junit.Test;

/**
 * @author Andre Dietisheim
 */
public class JBossToolsUsageRequestsTest {

	UsagePluginLogger logger = new UsagePluginLogger(JBossToolsUsageTestActivator.getDefault());
	
	// @Ignore
	// @Test
	// public void testUrl0() throws IOException {
	// String userAgent =
	// "Mozilla/5.0 (X11; U; Linux x86_64; th-TH; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) Firefox/10.0.0";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338321265"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_0() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338321288"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_0"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156032507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @IgnoreJBossToolsUsageRequestsTest
	// @Test
	// public void testUrl0_1() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) v201006010437R-H98-GA";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=3383212651"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_1"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030508.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_2() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) Eclipse/3.5.0";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=3383212652"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_2"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030509.1285760712.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_3() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH;) Eclipse/3.5.0";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=3383212651"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_3"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030501.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_4() throws IOException {
	// String userAgent = "com.jboss.jbds.product/3.0.1";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=3383212651"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_4"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030502.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_5() throws IOException {
	// String userAgent = "com.jboss.jbds.product/3.0.1 (Linux x86_64)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=33832126513"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_5"
	// + "&utmac=UA-17645367-1"
	// +
	// "&__utma%3D156030503.195542053.1281528584.1281528584.1281528584.1%3B%2B__utmz%3D156030500.1281528584.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_6() throws IOException {
	// String userAgent =
	// "Mozilla/5.0 (X11; U; Linux x86_64; th-TH; rv:1.9.2.4) Gecko/20100614 Ubuntu/10.04 (lucid) com.jboss.jbds.product/3.0.1";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338321265"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_6"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338321268"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_1() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338321068"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmje=1"
	// + "&utmfl=10.1%20r53"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_1"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156030507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_2() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=338333268"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=-%20JBoss%20Community"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_2"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D156620507.1285760111.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (X11; U; Linux x86_64; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=311333268"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D112660507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_mac() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Macintosh; U; Intel Mac OS X 10.5; fr)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351333268"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_mac"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D133660507.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351333254"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win"
	// + "&utmhid=1087431432"
	// + "&utmr=0"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D133660522.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_1() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=358333254"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_1"
	// + "&utmhid=1087431432"
	// + "&utmr=smooks|seam|drools|esb"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_1"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D133860522.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl8() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=453325272"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=jboss.org-tools-usage-instance"
	// + "&utmhid=1722580305"
	// + "&utmr=org.jboss.tools.usage.tests"
	// + "&utmp=%2Fjboss.org%2Ftools%2Fusage%2FtestUrl8"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.69517276658961975851281943564260.1281943564259.1281943564259.1281943564259.-1%3B%2B__utmz%3D999.1281943564259.1.1.utmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_referral() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351334444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win_referral"
	// + "&utmhid=1087431432"
	// + "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
	// + "&utm_content=test1%7Ctest2%7Ctest3"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_referral"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_adcontent() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=378334444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win_adcontent"
	// + "&utmhid=1087431432"
	// + "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
	// + "&utm_content=test1%7Ctest2%7Ctest3%7test4"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_adcontent"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D455663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_keyword() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=378334354"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win_keyword"
	// + "&utmhid=1087431432"
	// + "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
	// + "&term=test1%7Ctest2%7Ctest3%7test4"
	// + "&utm_term=test1a%7Ctest2a%7Ctest3a%7test4a"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_keyword"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D887463892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_utmz() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351334444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win__utmz"
	// + "&utmhid=1087431432"
	// + "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
	// + "&utm_content=test1%7Ctest2%7Ctest3"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win__utmz"
	// + "&utmac=UA-17645367-1"
	// + "&utmz=test1%7Ctest2%7Ctest3"
	// +
	// "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_utmctr() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351334444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win__utmctr"
	// + "&utmhid=1087431432"
	// + "&utmr=seam|esb|smooks|birt|bpel|cdi|deltacloud|drools"
	// + "&utm_content=test1%7Ctest2%7Ctest3"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_utctr"
	// + "&utmac=UA-17645367-1"
	// + "&utmz=test1%7Ctest2%7Ctest3"
	// +
	// "&utmcc=__utma%3D133663892.1285760711.1281430767.1281430767.1281430767.1%3B%2B__utmz%3D156030500.1281430767.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3D%7Cutmctr%3Dtest1%7Ctest2%7Ctest3%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	//
	// @Ignore
	// @Test
	// public void testUrl0_7_3_win_utmctr_lengthtest() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351334794"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-test_0_7_3_win_lengthtest"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl0_7_3_win_lengthtest"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D133697892.1285760711.1281430767.1281430767.1281430767.1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utmaCookies_0() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351358794"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_0"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_0"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// + "__utma%3D133697892.111.1281430767.1281430767.1281430767.1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * this test should create a request to the same url by a different eclipse
	 * instance
	 * 
	 * @throws IOException
	 */
	// @Test
	// public void testUrl_utmaCookies_0_otherEclipse() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351357694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_0"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_0"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// + "__utma%3D133697892.2222.1281430767.1281430767.1281430767.1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utmaCookies_1() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=261334794"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D133697892.1285760711.1281430767.1281430767.1281430867.2%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utmaCookies_1B() throws IOException {
	// String userAgent =
	// "com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)";
	// TestHttpGetMethod method = new TestHttpGetMethod(userAgent,
	// loggingAdapter);
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=261390794"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_1B"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_1B"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D133697892.1285760711.1281430767.1281430767.1281430867.2%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utmaCookies_2() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351784794"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_2"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_2"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D133697892.1285760711.1281430767.1281430867.1281430967.2%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utmaCookies_utmb_utmc_1() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=351789994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D131297892.1285760711.1281430767.1281430867.1281430967.2%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * this test should create a request from the same eclipse instance later in
	 * time (visit count increased, visit timestamps updated, userId identical)
	 */
	// @Test
	// public void testUrl_utmaCookies_utmb_utmc_1B() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=35176694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D131297892.1285760711.1281430767.1281430967.1281430988.3%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * this test should create a request from the same eclipse instance later in
	 * time (visit count increased, visit timestamps updated, userId identical)
	 */
	// @Test
	// public void testUrl_debug_utma() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=15176694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.5737734690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testUrl_utma_utmz() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=15176694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.5737734690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D999.1282924103925.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * FAILED
	 */
	// @Test // @Test
	// public void testUrl_debug_utma() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=15176694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=tools-usage-testUrl_utmaCookies_utmb_utmc_1"
	// + "&utmhid=1087431432"
	// + "&utmp=%2Ftools%2Fusage%2FtestUrl_utmaCookies_utmb_utmc_1"
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.5737734690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }
	// public void testJBossToolsVersionInRefererrer() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=818594305"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testJBossToolsVersionInRefererrer"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=testJBossToolsVersionInRefererrer"
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297294488397354581284108794036.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * FAILURE
	 */
	// @Test
	// public void testJBossToolsVersionInAdContent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod();
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=818594305"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testJBossToolsVersionInAdContent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=0"
	// + "&utm_content=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testJBossToolsVersionInAdContent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297294488397354581284108794036.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	enum UserAgentString {

		DEFAULT("com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)"),
		WIN7("com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.1; th-TH)"),
		WINVISTA("com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 6.0; th-TH)"),
		WINXP("com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 5.1; th-TH)"),
		WIN2000("com.jboss.jbds.product/3.0.1 (Windows; U; Windows NT 5.0; th-TH)"),
		MACOS_SNOWLEO("com.jboss.jbds.product/3.0.1 (Macintosh; U; Intel Mac OS X 10.6; th-TH)"),
		MACOS_LEO("com.jboss.jbds.product/3.0.1 (macintosh; U; Intel Mac OS X 10.5; th-TH)");

		private String userAgent;

		UserAgentString(String userAgent) {
			this.userAgent = userAgent;
		}

		public String toString() {
			return userAgent;
		}
	}

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testWinWin7UserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(UserAgentString.WIN7);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=9115966544326"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testWinWin7UserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testWinWin7UserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297885388356354581284108794036.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testWinVistaUserAgent() throws IOException {
	// TestHttpGetMethod method = new
	// TestHttpGetMethod(UserAgentString.WINVISTA);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=9586654334566"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testWinVistaUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testWinVistaUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.432978853883973545812841087555446.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testWinXPUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(UserAgentString.WINXP);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=958593335"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testWinXPUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new JBossToolsTestsFocusPoint("testWinXPUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297245388397354581284544794036.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testWin2000UserAgent() throws IOException {
	// TestHttpGetMethod method = new
	// TestHttpGetMethod(UserAgentString.WIN2000);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=9588949905"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testWin2000UserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testWin2000UserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297292388397354581284009098776666.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testMacLeopardUserAgent() throws IOException {
	// TestHttpGetMethod method = new
	// TestHttpGetMethod(UserAgentString.MACOS_LEO);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=958890665"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testMacLeopardUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testMacLeopardUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.432972923883973335812841778899886.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * SUCCESS
	 */
	// @Test
	// public void testMacSnowLeopardUserAgent() throws IOException {
	// TestHttpGetMethod method = new
	// TestHttpGetMethod(UserAgentString.MACOS_SNOWLEO);
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=958890444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testMacSnowLeopardUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp=" + new
	// JBossToolsTestsFocusPoint("testMacSnowLeopardUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.432972923883973335812221087554396.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxFedoraUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.8.1.14) Gecko/20080612 Fedora/2.0.0.14-20080612.fc8.acer Firefox/2.0.0.14");
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=95830066444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxFedoraUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testLinuxFedoraUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.43297292388397333777722108766778.1284108997725.1284198794990.1284108794995.1%3B%2B__utmz%3D999.1284108794025.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxUbuntuUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; pl-PL; rv:1.9.0.2) Gecko/20121223 Ubuntu/9.25 (jaunty) Firefox/3.8");
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=958990044"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxUbuntuUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testLinuxUbuntuUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.46667292388397333777722108794036.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794225.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSUSEUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux x86_64; th-TH; rv:1.9.2.8) Gecko/20100723 SUSE/3.6.8-0.1.1 Firefox/3.6.8");
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=957000994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSUSEUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testLinuxSUSEUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.46667292388397333777722108798936.1284108794025.1284108794025.1284108794025.1%3B%2B__utmz%3D999.1284108794225.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSlackwareUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux x86_64; th-TH; rv:1.9.1.3) Gecko/20090914 Slackware/13.0_stable Firefox/3.5.3");
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=98888444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSlackwareUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testLinuxSlackwareUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.46667292388397333777722108798936.1284108996025.1284197794025.1284108794025.1%3B%2B__utmz%3D999.1284108794225.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxGentooUserAgent() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.9.1.3) Gecko/20090912 Gentoo Firefox/3.5.3 FirePHP/0.3");
	// String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
	// + "&utmn=957077444"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxGentooUserAgent"
	// + "&utmfl=1.6.0_20"
	// + "&utmr=1.0.0.qualifier"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testLinuxGentooUserAgent").getURI()
	// + "&utmac=UA-17645367-1"
	// +
	// "&utmcc=__utma%3D999.46667292388397333777722108798936.1284108794025.1284108788442.1284108880025.1%3B%2B__utmz%3D999.1284108794225.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7Cutmctr%3DVPE-%EF%BF%BF%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSimplifiedGentooUserAgent1() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.9.1.3) Gecko/20090912 Gentoo Firefox/3.5.3");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=22172214"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSimplifiedGentooUserAgent1"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new
	// JBossToolsTestsFocusPoint("testLinuxSimplifiedGentooUserAgent1").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.44677790471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSimplifiedGentooUserAgent2() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.9.1.3) Gecko/20090912 Gentoo");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=1517776694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSimplifiedGentooUserAgent2"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new
	// JBossToolsTestsFocusPoint("testLinuxSimplifiedGentooUserAgent2").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.3332224690471263281282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSimplifiedGentooUserAgent3() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.9.1.3) Gentoo");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=66543336546694"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSimplifiedGentooUserAgent3"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new
	// JBossToolsTestsFocusPoint("testLinuxSimplifiedGentooUserAgent3").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.333222469055434512381282924103927.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testLinuxSimplifiedFedoraUserAgent1() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (X11; U; Linux i686; th-TH; rv:1.9.1.3) Gecko/20090912 Fedora release 13 (Goddard)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=1517779994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testLinuxSimplifiedFedoraUserAgent1"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new
	// JBossToolsTestsFocusPoint("testLinuxSimplifiedFedoraUserAgent1").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.3332224690455442812829241027.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testKonquerorUserAgent1() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (compatible; Konqueror/3.5; Linux 2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=1223329994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testKonquerorUserAgent1"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testKonquerorUserAgent1").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.33322242235555442812829241027.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testKonquerorUserAgent1B() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (compatible; Konqueror/3.5; Linux Fedora13; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=1223329994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testKonquerorUserAgent1B"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testKonquerorUserAgent1B").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.33322242235555442812829241027.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// /**
	// * FAILURE on 29.9. SUCCESS on 30.9 (how odd!!)
	// */
	// @Test
	// public void testKonquerorUserAgent1C() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "JBossToolsTest/5.0 (compatible; Konqueror/3.5; Linux 2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=1213328994"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testKonquerorUserAgent1C"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testKonquerorUserAgent1C").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.33554422242235555442817769241027.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testKonquerorUserAgent2() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (compatible; Konqueror/3.5; Linux Fedora13; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=122366977894"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testKonquerorUserAgent2"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testKonquerorUserAgent2").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.33327772235565334281282977327.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	// @Test
	// public void testKonquerorUserAgent3() throws IOException {
	// TestHttpGetMethod method = new TestHttpGetMethod(
	// "Mozilla/5.0 (compatible; Konqueror/3.5; Linux Fedora13; X11) KHTML/3.5.3 (like Gecko)");
	// String url = "http://www.google-analytics.com/__utm.gif?"
	// + "utmwv=4.7.2"
	// + "&utmn=126732977894"
	// + "&utmhn=jboss.org"
	// + "&utmcs=UTF-8"
	// + "&utmsr=1920x1080"
	// + "&utmsc=24-bit"
	// + "&utmul=th-TH"
	// + "&utmdt=testKonquerorUserAgent3"
	// + "&utmhid=1087431432"
	// + "&utmp="
	// + new JBossToolsTestsFocusPoint("testKonquerorUserAgent3").getURI()
	// + "&utmac=UA-17645367-1"
	// + "&utmcc="
	// +
	// "__utma%3D999.33322242235565388981282924327.1282924103925.1282924103925.1282924103925.1%3B%2B"
	// + "__utmb%3D1%3B%2B"
	// + "__utmc%3D1%3B%2B"
	// + "__utmz%3D156030500.1281430767.1.1."
	// + "utmcsr%3D(direct)%7C"
	// + "utmccn%3D(direct)%7C"
	// + "utmcmd%3D(none)%7C"
	// +
	// "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
	// + "&gaq=1";
	// method.request(url);
	// assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	// }

	/**
	 * FAILURE on 29.9. SUCCESS on 30.9 (how odd!!)
	 */
	@Test
	public void testFedora1() throws IOException {
		TestHttpGetMethod method = new TestHttpGetMethod(
				"JBossToolsTest/5.0 (compatible; Konqueror/3.5; Linux 2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=1213328994"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=th-TH"
				+ "&utmdt=testFedora1"
				+ "&utmhid=1087431432"
				+ "&utmp="
				+ new JBossToolsTestsFocusPoint("testFedora1").getURI()
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+
				"__utma%3D999.33554422242235555442817769241027.1282924103925.1282924103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
				+ "utmcsr%3D(direct)%7C"
				+ "utmccn%3D(direct)%7C"
				+ "utmcmd%3D(none)%7C"
				+
				"utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testFedora2B() throws IOException {
		TestHttpGetMethod method = new TestHttpGetMethod(
				"JBossToolsTest/5.0 (compatible; Konqueror/3.5; Linux Fedora2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=1213322444"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=th-TH"
				+ "&utmdt=testFedora2B"
				+ "&utmhid=1087431432"
				+ "&utmp="
				+ new JBossToolsTestsFocusPoint("testFedora2B").getURI()
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+
				"__utma%3D999.542543254325423542367678888899.1282924103925.1282924103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
				+ "utmcsr%3D(direct)%7C"
				+ "utmccn%3D(direct)%7C"
				+ "utmcmd%3D(none)%7C"
				+ "__utmv%3Distro.Fedora13%7C"
				+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testKonquerorUserAgent1E() throws IOException {
		TestHttpGetMethod method = new TestHttpGetMethod(
				"JBossToolsTest/5.0 (compatible; Konqueror/3.5; Linux Fedora2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=22566794"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=th-TH"
				+ "&utmdt=testKonquerorUserAgent1E"
				+ "&utmhid=1087431432"
				+ "&utmp="
				+ new JBossToolsTestsFocusPoint("testKonquerorUserAgent1E").getURI()
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+
				"__utma%3D999.556425245435468874422899.1282924103925.1282924103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
				+ "utmcsr%3D(direct)%7C"
				+ "utmccn%3D(direct)%7C"
				+ "utmcmd%3D(none)%7C"
				+ "__utmv%3Distro.Fedora13"
				+ "&utmcc=__utma%3D999.75104087789840654911285868550350.1285868546766.1285868546766.1285868546766.1%3B%2B__utmz%3D999.1285868546766.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7C__utmv%3D1917863289.CentOS13%EF%BF%BF%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}

	@Test
	public void testKonquerorUserAgent1F() throws IOException {
		TestHttpGetMethod method = new TestHttpGetMethod(
				"JBossToolsTest/5.0 (compatible; Konqueror/3.5; Linux 2.6.14-kanotix-6; X11) KHTML/3.5.3 (like Gecko) (Debian package 4:3.5.3-1)");
		String url = "http://www.google-analytics.com/__utm.gif?"
				+ "utmwv=4.7.2"
				+ "&utmn=2277888794"
				+ "&utmhn=jboss.org"
				+ "&utmcs=UTF-8"
				+ "&utmsr=1920x1080"
				+ "&utmsc=24-bit"
				+ "&utmul=th-TH"
				+ "&utmdt=testKonquerorUserAgent1F"
				+ "&utmhid=1087431432"
				+ "&utmp="
				+ new JBossToolsTestsFocusPoint("testKonquerorUserAgent1F").getURI()
				+ "&utmac=UA-17645367-1"
				+ "&utmcc="
				+
				"__utma%3D999.55642524543567654765765899.1282333925.12823334103925.1282924103925.1%3B%2B"
				+ "__utmb%3D1%3B%2B"
				+ "__utmc%3D1%3B%2B"
				+ "__utmz%3D156030500.1281430767.1.1."
				+ "utmcsr%3D(direct)%7C"
				+ "utmccn%3D(direct)%7C"
				+ "utmcmd%3D(none)%7C"
				+ "__utmv%3Distro=Fedora13"
				+ "utmctr%3Dtest1%7Ctest2%7Ctest3%7Ctest4%7Ctest5%7Ctest6%7Ctest7%7Ctest8%7Ctest8%7Ctest9%7Ctest10%7Ctest11%7Ctest12%7Ctest13%7Ctest514%7Ctest14%7Ctest15%7Ctest16%7Ctest17%7Ctest18%7Ctest19%7Ctest20%7Ctest20%7Ctest21%7Ctest22%7Ctest23%7Ctest514%7Ctest24%7Ctest25%7Ctest26%7Ctest27%7Ctest28%7Ctest29%7Ctest30%7Ctest31%3B"
				+ "&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}
	
	@Test
	public void verifyCurrentReportingRequest() throws IOException {
		TestHttpGetMethod method = new TestHttpGetMethod(
				"JBossToolsTest/6.0 (compatible; Konqueror/3.5; Linux 2.6.14-fc13; X11) KHTML/3.5.3 (like Gecko) (Fedora)");
		String url = "http://www.google-analytics.com/__utm.gif?utmwv=4.7.2"
			+"&utmn=363487156"
			+"&utmhn=jboss.org"
			+"&utmcs=UTF-8"
			+"&utmsr=2880x1024"
			+"&utmsc=24-bit"
			+"&utmul=en-US"
			+"&utmdt=verifyCurrentReportingRequest"
			+"&utmfl=1.6.0_21"
			+"&utmr=0"
			+"&utmp=" + new JBossToolsTestsFocusPoint("verifyCurrentReportingRequest").getURI()
			+"&utmac=UA-17645367-1"
			+"&utmcc=__utma%3D999.32635895694074051961285867957380.1285867856601.1285867856601.1285867856601.1%3B%2B__utmz%3D999.1285867856601.1.1.%EF%BF%BFutmcsr%3D%28direct%29%7Cutmccn%3D%28direct%29%7Cutmcmd%3D%28none%29%7C__utmv%3DCentOS13%EF%BF%BF%3B"
			+"&gaq=1";
		method.request(url);
		assertEquals(HttpURLConnection.HTTP_OK, method.getResponseCode());
	}
	
	
	protected class TestHttpGetMethod extends HttpGetRequest {

		private HttpURLConnection urlConnection;

		public TestHttpGetMethod(String userAgentString) {
			this(userAgentString, logger);
		}

		public TestHttpGetMethod(UserAgentString userAgentString) {
			this(userAgentString.toString(), logger);
		}

		public TestHttpGetMethod() {
			super(UserAgentString.DEFAULT.toString(), logger);
		}

		public TestHttpGetMethod(String userAgent, UsagePluginLogger logger) {
			super(userAgent, logger);
		}

		@Override
		protected HttpURLConnection createURLConnection(String urlString,
				String userAgent) throws IOException {
			return this.urlConnection = super.createURLConnection(urlString, userAgent);
		}

		public int getResponseCode() throws IOException {
			return urlConnection.getResponseCode();
		}
	}
}
